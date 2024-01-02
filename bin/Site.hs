{-# LANGUAGE OverloadedStrings #-}
module Site
  ( site
  ) where

import Control.Monad (foldM)
import Data.Maybe (fromMaybe, isJust)
import Data.List (isPrefixOf)
import System.Directory
import System.FilePath
import Text.Pandoc.Options
import Text.Pandoc.SideNote (usingSideNotes)

import Hakyll
import Hakyll.Core.Compiler.Internal

baseRules :: Rules ()
baseRules = do
  match "css/*.css" $ do
    route idRoute
    compile copyFileCompiler
  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler
  match "assets/*" $ do
    route idRoute
    compile copyFileCompiler
  match "tufte/*" $ do
    route idRoute
    compile copyFileCompiler
  match "templates/*" $ compile templateCompiler

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
        usingSideNotes
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

noteCompiler :: Compiler (Item String)
noteCompiler = do
  body <- getResourceBody
  content <-
    applyAsTemplate
      (defaultContext <> basenameContext <> transcludeContext)
      body
  p <- pandocWithSidenotes content
  let ident = itemIdentifier p
  imd <- getMetadata ident
  let links = maybe [] words (lookupString "related" imd)
  items <- mapM f links
  let context =
        defaultContext
          <> listField
               "links"
               (defaultContext <> basenameContext)
               (return items)
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

transcludeContext :: Context a
transcludeContext =
  functionField "transclude" $ \args _ ->
    case args of
      [id'] ->
        compilerUnsafeIO
          $ transclude id' <$> readFile ("forest/" ++ id' ++ ".md")
      _ -> fail "transclude should receive a single argument"
  where
    transclude id' content =
      let title = yankTitle content
       in "<span class=\"transclusion-title\">"
            ++ trim' title
            ++ "</span> <span class=\"transclusion-link\">[["
            ++ id'
            ++ "](/"
            ++ id'
            ++ ".html"
            ++ ")]</span>"
            ++ dropFM (drop 3 content)
    dropFM cs =
      if take 3 cs == "---"
        then drop 3 cs
        else dropFM (drop 3 cs)

site :: Rules ()
site = do
  baseRules
  match "forest/dsp-0001.md" $ do
    route (constRoute "index.html")
    compile noteCompiler
  match "forest/*.md" $ do
    route (setExtension "html")
    compile noteCompiler
