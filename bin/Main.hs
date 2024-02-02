{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Char (chr)
import Data.List (singleton)
import Options.Applicative
  ( Parser
  , (<**>)
  , command
  , execParser
  , fullDesc
  , helper
  , info
  , progDesc
  , subparser
  )
import System.Directory (doesFileExist)
import System.Process (callCommand)
import System.Random ( randomRIO )

import Hakyll
  ( Configuration
  , Options(..)
  , defaultConfiguration
  , destinationDirectory
  , hakyllWithExitCodeAndArgs
  )
import Hakyll.Core.Runtime ( RunMode(RunModeNormal) )
import qualified Hakyll.Main as H
  
import Site (site)

data Commands
  = New
  | Build
  | Rebuild
  | Serve

newParser :: Parser Commands
newParser = pure New

buildParser :: Parser Commands
buildParser = pure Build

rebuildParser :: Parser Commands
rebuildParser = pure Rebuild

serveParser :: Parser Commands
serveParser = pure Serve

mainParser :: Parser Commands
mainParser =
  subparser
    $ command "new" (info newParser (progDesc "create a new note"))
        <> command "build" (info buildParser (progDesc "build the site"))
        <> command "rebuild" (info buildParser (progDesc "rebuild the site"))
        <> command
             "serve"
             (info serveParser (progDesc "run the server and watch for changes"))

printNewPath :: IO ()
printNewPath = do
  ran1 <- ranDigit
  ran2 <- ranChar
  ran3 <- ranDigit
  ran4 <- ranChar
  let path = "forest/dsp-" ++ ran1 ++ ran2 ++ ran3 ++ ran4 ++ ".md"
  exists <- doesFileExist path
  if exists
    then printNewPath
    else putStrLn path
  where
    ranDigit = show <$> (randomRIO (0, 9) :: IO Int)
    ranChar = singleton . chr <$> randomRIO (65, 90)

runHakyll :: Configuration -> Options -> IO ()
runHakyll conf opts = void $ hakyllWithExitCodeAndArgs conf opts site

main = do
  let config = defaultConfiguration {destinationDirectory = "/tmp/dpitt-site"}
  cmd <- execParser (info (mainParser <**> helper) fullDesc)
  case cmd of
    New -> printNewPath
    Rebuild -> do
      runHakyll config (Options False H.Rebuild)
      callCommand "cp -r /tmp/dpitt-site/* _site/ || true"
      callCommand "rm -rf /tmp/dpitt-site/*"
    Build -> do
      runHakyll config (Options False (H.Build RunModeNormal))
      callCommand "cp -r /tmp/dpitt-site/* _site/ || true"
      callCommand "rm -rf /tmp/dpitt-site/*"
    Serve -> do
      runHakyll config (Options False (H.Build RunModeNormal))
      callCommand "cp -r /tmp/dpitt-site/* _site/ || true"
      callCommand "rm -rf /tmp/dpitt-site/*"
      runHakyll
        defaultConfiguration
        (Options False (H.Watch "localhost" 8000 False))
