{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (foldM)
import Data.Maybe (fromMaybe, isJust)
import Text.Pandoc.Options
import Text.Pandoc.SideNote (usingSideNotes)

import Data.Char (toUpper)

import Hakyll

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

noteCompiler :: Compiler (Item String)
noteCompiler = do
  p <- pandocWithSidenotes
  let ident = itemIdentifier p
  imd <- getMetadata ident
  let links = maybe [] words (lookupString "related" imd)
  items <- mapM f links
  let context =
        defaultContext <> listField "links" defaultContext (return items)
  loadAndApplyTemplate "templates/base.html" context p
  where
    f id = load (fromFilePath ("forest/" ++ id))

main :: IO ()
main =
  hakyll $ do
    baseRules
    match "forest/dsp-0001.md" $ do
      route (constRoute "index.html")
      compile noteCompiler
    match "forest/*.md" $ do
      route (setExtension "html")
      compile noteCompiler
