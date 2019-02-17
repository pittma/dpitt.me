{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll

import System.FilePath

import Text.Pandoc.Options
import Text.Pandoc.SideNote (usingSideNotes)

main :: IO ()
main =
  hakyll $ do
    match "tufte/et-book/*/*" $ route $ customRoute $ drop 6 . toFilePath
    match "tufte/tufte.css" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*.css" $ do
      route idRoute
      compile copyFileCompiler
    match "js/*.js" $ do
      route idRoute
      compile copyFileCompiler
    match "posts/*" $ do
      route $ toIdxPath "blog"
      compile $
        pandocWithSidenotes >>=
        loadAndApplyTemplate "templates/default.html" dateCtx
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- take 5 <$> (recentFirst =<< loadAll "posts/*")
        let context = listField "posts" (dateCtx <> cleanUrlCtx) (return posts)
        getResourceBody >>= applyAsTemplate context >>=
          loadAndApplyTemplate "templates/default.html" defaultContext
    match "templates/*" $ compile templateCompiler

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y" <> defaultContext

cleanUrlCtx :: Context String
cleanUrlCtx = field "clean-url" (return . clean . toFilePath . itemIdentifier)
  where
    clean path = takeDirectory path </> takeBaseName path

-- This is the infamous `niceRoute' function.
toIdxPath :: String -> Routes
toIdxPath prefix = customRoute createIndexRoute
  where
    createIndexRoute ident =
      let path = toFilePath ident
       in prefix </> takeDirectory path </> takeBaseName path </> "index.html"

pandocWithSidenotes :: Compiler (Item String)
pandocWithSidenotes =
  let defWExt = writerExtensions defaultHakyllWriterOptions
      mathExtensions = [Ext_tex_math_dollars, Ext_latex_macros]
      extents = foldr enableExtension defWExt mathExtensions
      wopts =
        defaultHakyllWriterOptions
          {writerExtensions = extents, writerHTMLMathMethod = MathJax ""}
   in pandocCompilerWithTransform
        defaultHakyllReaderOptions
        wopts
        usingSideNotes
