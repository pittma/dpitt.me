{-# LANGUAGE OverloadedStrings, TypeApplications, DeriveGeneric #-}
module Site where

import Data.List (isPrefixOf, sortBy)
import Data.Ord (Down(Down), comparing)
import qualified Data.Text as T
import Data.Time
import Dhall (FromDhall)
import GHC.Generics (Generic)
import System.FilePath
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk
import Text.Pandoc.SideNote (usingSideNotes)

import Hakyll
import Hakyll.Core.Compiler.Internal

data PittConfig = Cfg
  { cfg_index :: !String
  , cfg_cv :: !String
  , cfg_garage_note :: !String
  } deriving (Generic)

instance FromDhall PittConfig

mkIndex :: PittConfig -> Pattern
mkIndex c = fromGlob ("forest/" ++ cfg_index c ++ ".md")

baseCtx :: PittConfig -> Context String
baseCtx c = constField "garage-note" (cfg_garage_note c) <> defaultContext

baseRules :: Rules ()
baseRules = do
  match "css/*.css" $ do
    route idRoute
    compile copyFileCompiler
  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler
  match "files/*" $ do
    route idRoute
    compile copyFileCompiler
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler
  match "tufte/*" $ do
    route idRoute
    compile copyFileCompiler
  match "trex/*/*" $ do
    route idRoute
    compile copyFileCompiler
  match "trex/*/*/*" $ do
    route idRoute
    compile copyFileCompiler
  match "templates/*" $ compile templateCompiler

fencedDivs :: Pandoc -> Pandoc
fencedDivs =
  walk $ \block ->
    case block of
      (Div (x, cls, y) content) -> handleDiv block x cls y content
      _otherwise -> block
  where
    handleDiv block x cls y content
      | "note" `elem` cls = note x cls y content
      | "insight" `elem` cls = insight x cls y content
      | otherwise = block
    note x cls y content =
      Div
        (x, cls, y)
        (Div ("", ["note-header"], []) [Plain [Str "ⓘ Note"]] : content)
    insight x cls y content =
      Div
        (x, cls, y)
        (Div ("", ["insight-header"], []) [Plain [Str "🧠 Insight"]] : content)
  
pandocWithSidenotes :: Item String -> Compiler (Item String)
pandocWithSidenotes item =
  let defWExt = writerExtensions defaultHakyllWriterOptions
      mathExtensions = [Ext_tex_math_dollars, Ext_latex_macros]
      extents = foldr enableExtension defWExt mathExtensions
      wopts =
        defaultHakyllWriterOptions
          {writerExtensions = extents, writerHTMLMathMethod = MathJax ""}
   in renderPandocWithTransform
        defaultHakyllReaderOptions
        wopts
        (fencedDivs . usingSideNotes)
        item

trim' :: String -> String
trim' cs
  | head cs == '"' && last cs == '"' = tail (init cs)
  | head cs == '"' = tail cs
  | last cs == '"' = init cs
  | otherwise = cs

yankTitle :: String -> String
yankTitle content@(_:content') =
  if "title: " `isPrefixOf` content
    then trim' $ toNewLine (drop 7 content)
    else yankTitle content'
  where
    toNewLine (c:cs) =
      if c == '\n'
        then []
        else c : toNewLine cs
    toNewLine [] = []
yankTitle [] = []

noteCompiler :: PittConfig -> Tags -> Compiler (Item String)
noteCompiler c tags = do
  body <- getResourceBody
  content <-
    applyAsTemplate
      (basenameContext <> transcludeContext <> baseCtx c)
      body
  p <- pandocWithSidenotes content
  p' <- saveSnapshot "notes" p
  let ident = itemIdentifier p'
  imd <- getMetadata ident
  let links = maybe [] words (lookupString "related" imd)
  items <- mapM f links
  let context =
        generatedContext
          <> tagsField "tags" tags
          <> (if not (null items)
                then listField
                       "links"
                       (baseCtx c <> basenameContext)
                       (return items)
                else mempty)
          <> baseCtx c
  loadAndApplyTemplate "templates/base.html" context p
  where
    f id' =
      compilerUnsafeIO $ do
        let path = "forest/" ++ id' ++ ".md"
        content <- readFile path
        return (Item (fromFilePath path) content)
      
basenameContext :: Context a
basenameContext =
  field "basename" (return . takeBaseName . toFilePath . itemIdentifier)

generatedContext :: Context a
generatedContext =
  field "generated" $ \_ ->
    compilerUnsafeIO $ show . localDay . zonedTimeToLocalTime <$> getZonedTime

transcludeContext :: Context a
transcludeContext =
  functionField "transclude" $ \args _ ->
    case args of
      [id'] ->
        compilerUnsafeIO
          $ transclude id' <$> readFile ("forest/" ++ id' ++ ".md")
      _otherwise -> fail "transclude should receive a single argument"
  where
    transclude id' content =
      let title = yankTitle content
          content' = dropFM (drop 3 content)
          content'' = T.replace "$$" "$" (T.pack content')
       in "<hr class='transclude-hr'/>\n<span class=\"transclusion-title\">"
            ++ title
            ++ "</span> <span class=\"transclusion-link\">[["
            ++ id'
            ++ "](/forest/"
            ++ id'
            ++ ".html"
            ++ ")]</span>"
            ++ T.unpack content''
            ++ "\n<hr class='transclude-hr' />"
    dropFM content@(_:cs) =
      if take 3 content == "---"
        then drop 3 content
        else dropFM cs
    dropFM [] = []


tagsContext :: PittConfig -> Tags -> Context String
tagsContext c tags = tagsField "tags" tags <> baseCtx c <> basenameContext

tagListContext :: Tags -> Context a
tagListContext tags =
  let items = sortBy (comparing (Down . length . snd)) (tagsMap tags)
      items' = mapM (makeItem . f) items
   in listField "tagList" (tag' <> count') items'
  where
    f (tag, posts) = tag ++ "," ++ show (length posts)
    tag' = field "tag" $ \(Item _ body) -> return (toComma body)
    count' = field "count" $ \(Item _ body) -> return (fromComma body)
    toComma (',':_) = []
    toComma (c:cs) = c : toComma cs
    toComma [] = []
    fromComma (',':cs) = cs
    fromComma (_:cs) = fromComma cs
    fromComma [] = []

url :: Identifier -> FilePath
url ident =
  let path = toFilePath ident
      bn = takeBaseName path
   in takeDirectory path </> (bn ++ ".html")

urlContext :: Context String
urlContext =
  field "url" (return . url . itemIdentifier)

tagsListField :: Context a
tagsListField =
  listFieldWith "tags-list" tagCtx $ \item ->
    getTags (itemIdentifier item) >>= mapM makeItem
  where
    tagCtx = field "tag" (return . itemBody)

pubDateField :: Context a
pubDateField =
  field "pubDate" $ \item -> do
    (Just dateStr) <- getMetadataField (itemIdentifier item) "published"
    pure (toRfc822 dateStr)

generatedField :: Context a
generatedField =
  field "generated" $ \_ ->
    compilerUnsafeIO
      $ toRfc822 . show . localDay . zonedTimeToLocalTime <$> getZonedTime

toRfc822 :: String -> String
toRfc822 ds =
  let (Just date) =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" ds :: Maybe UTCTime
   in formatTime defaultTimeLocale rfc822DateFormat date
  
site :: PittConfig -> Rules ()
site c = do
  baseRules
  tags <- buildTags "forest/*" (fromCapture "tags/*/index.html")
  tagsRules tags $ \tag pat -> do
    let title = "Posts tagged <i>" ++ tag ++ "</i>"
    route idRoute
    compile $ do
      posts <- loadAll pat
      let context =
            constField "title" title
              <> listField "items" (tagsContext c tags) (return posts)
      makeItem @String "" >>= loadAndApplyTemplate "templates/tags.html" context
  match (mkIndex c) $ do
    route (constRoute "index.html")
    compile $ noteCompiler c tags
  match "forest/*.md" $ do
    route (setExtension "html")
    compile $ noteCompiler c tags
  match "tags.html" $ do
    route idRoute
    compile $ getResourceBody >>= applyAsTemplate (tagListContext tags)
  match "feed.xml" $ do
    route idRoute
    compile $ do
      notes <- recentFirst =<< loadAllSnapshots "forest/*.md" "notes"
      let context =
            defaultContext
              <> generatedField
              <> listField
                   "items"
                   (defaultContext
                      <> generatedField
                      <> urlContext
                      <> tagsListField
                      <> pubDateField)
                   (pure notes)
      getResourceBody >>= applyAsTemplate context
