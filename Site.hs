{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import HakyllHacks

import System.FilePath
import Text.Pandoc.Options
import Text.Pandoc.SideNote (usingSideNotes)

main :: IO ()
main =
  hakyllWithBaseRules $ do

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

    match "templates/*" $ compile templateCompiler


    match "cv.pdf" $ do
      route $ constRoute "cv/cv.pdf"
      compile copyFileCompiler
    
    match "cv.md" $ do
      route toIdxPath
      compile $ do
        getResourceBody >>=
          renderPandoc >>=
          loadAndApplyTemplate "templates/cv.html" defaultContext
      
    match "index.html" $ do
      route idRoute
      compile $ asPostTemp defaultContext

asPostTemp :: Context String -> Compiler (Item String)
asPostTemp = asTempWithDefault "templates/post.html"

composeTeaser :: String -> Context String
composeTeaser = teaserFieldWithSeparator "···" "teaser"

cleanRouteCtx :: Context String
cleanRouteCtx =
  field "clean-route" (return . clean . toFilePath . itemIdentifier)
  where
    clean path = takeDirectory path </> takeBaseName path
