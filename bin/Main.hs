{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Bool
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

randString :: Int -> IO String
randString len = concat <$> sequence (f len [])
  where
    f 0 s = s
    f n s = f (n - 1) (bool (ranDigit : s) (ranChar : s) (even n))
    ranDigit = show <$> (randomRIO (0, 9) :: IO Int)
    ranChar = singleton . chr <$> randomRIO (65, 90)

printNewPath :: IO ()
printNewPath = do
  r <- randString 4
  let path = "forest/dsp-" ++ r ++ ".md"
  exists <- doesFileExist path
  if exists
    then printNewPath
    else putStrLn path

runHakyll :: Configuration -> Options -> IO ()
runHakyll conf opts = void $ hakyllWithExitCodeAndArgs conf opts site

main :: IO ()
main = do
  let config = defaultConfiguration {destinationDirectory = "/tmp/dpitt-site"}
  cmd <- execParser (info (mainParser <**> helper) fullDesc)
  case cmd of
    New -> printNewPath
    Rebuild -> do
      runHakyll config (Options False H.Rebuild)
      callCommand "rm -rf _cache"
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
