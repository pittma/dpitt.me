{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import HakyllHacks

import System.FilePath
import Text.Pandoc.Options

main :: IO ()
main =
  hakyllWithBaseRules $ do
    match "tufte/et-book/*/*" $ route $ customRoute $ drop 6 . toFilePath
    match "tufte/tufte.css" $ do
      route idRoute
      compile copyFileCompiler
    match "blog/*" $ do
      route slugRoute
      compile $
        pandocWithSidenotes >>=
        loadAndApplyTemplate "templates/post.html" (dateCtx <> defaultContext)
    match "talks/*" $ do
      route toIdxPath
      compile $
        getResourceBody >>= applyAsTemplate defaultContext >>=
        saveSnapshot "talk-content" >>=
        renderPandoc >>=
        loadAndApplyTemplate "templates/post.html" defaultContext
    match "talks.html" $ do
      route toIdxPath
      compile $ do
        talks <- recentFirst =<< loadAll "talks/*"
        let context =
              listField
                "talks"
                (defaultContext <> composeTeaser "talk-content" <>
                 cleanRouteCtx)
                (return talks)
        asPostTemp context
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "blog/*"
        let context = listField "items" (dateCtx <> blogRouteCtx <> defaultContext) (return posts)
        getResourceBody >>= applyAsTemplate context
    match "newsletter.html" $ do
      route toIdxPath
      compile $ do
        getResourceBody >>= applyAsTemplate defaultContext
    match "templates/*" $ compile templateCompiler

    match "*.html" $ do
      route toIdxPath
      compile $ asPostTemp defaultContext

asPostTemp :: Context String -> Compiler (Item String)
asPostTemp = asTempWithDefault "templates/post.html"

composeTeaser :: String -> Context String
composeTeaser = teaserFieldWithSeparator "···" "teaser"

pandocWithSidenotes :: Compiler (Item String)
pandocWithSidenotes =
  let defWExt = writerExtensions defaultHakyllWriterOptions
      mathExtensions = [Ext_tex_math_dollars, Ext_latex_macros]
      extents = foldr enableExtension defWExt mathExtensions
      wopts =
        defaultHakyllWriterOptions
          {writerExtensions = extents, writerHTMLMathMethod = MathJax ""}
   in pandocCompilerWith
        defaultHakyllReaderOptions
        wopts

blogRouteCtx :: Context String
blogRouteCtx =
  field "blog-route" (return . dropFileName . dateSlug . itemIdentifier)

cleanRouteCtx :: Context String
cleanRouteCtx =
  field "clean-route" (return . clean . toFilePath . itemIdentifier)
  where
    clean path = takeDirectory path </> takeBaseName path
