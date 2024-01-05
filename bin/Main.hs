{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.List
import Options.Applicative
import System.Directory
import System.Process

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
  | Serve

newParser :: Parser Commands
newParser = pure New

buildParser :: Parser Commands
buildParser = pure Build

serveParser :: Parser Commands
serveParser = pure Serve

mainParser :: Parser Commands
mainParser =
  subparser
    $ command "new" (info newParser (progDesc "create a new note"))
        <> command "build" (info buildParser (progDesc "build the site"))
        <> command
             "serve"
             (info serveParser (progDesc "run the server and watch for changes"))

printNewPath :: IO ()
printNewPath = do
  c <- getDirectoryContents "./forest/"
  let next = length (filter isNote c) + 1
  putStrLn ("forest/dsp-" ++ format next ++ ".md")
  where
    isNote path = "dsp-" `isPrefixOf` path
    format count
      | count < 10 = "000" ++ show count
      | count < 100 = "00" ++ show count
      | count < 1000 = "0" ++ show count
      | otherwise = show count

runHakyll :: Configuration -> Options -> IO ()
runHakyll conf opts = void $ hakyllWithExitCodeAndArgs conf opts site

main = do
  let config = defaultConfiguration {destinationDirectory = "/tmp/dpitt-site"}
  cmd <- execParser (info (mainParser <**> helper) fullDesc)
  case cmd of
    New -> printNewPath
    Build -> do
      runHakyll config (Options False H.Rebuild)
      callCommand "cp -r /tmp/dpitt-site/* _site/"
      callCommand "rm -rf /tmp/dpitt-site/*"
    Serve -> do
      runHakyll config (Options False H.Rebuild)
      callCommand "cp -r /tmp/dpitt-site/* _site/"
      callCommand "rm -rf /tmp/dpitt-site/*"
      runHakyll
        defaultConfiguration
        (Options False (H.Watch "localhost" 8000 False))
