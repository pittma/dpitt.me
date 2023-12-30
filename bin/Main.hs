module Main where

import Data.List
import Hakyll
  ( Command(Rebuild, Watch)
  , Options(..)
  , defaultConfiguration
  , hakyllWithArgs
  )
import Options.Applicative
import System.Directory
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

runHakyll :: Options -> IO ()
runHakyll opts = hakyllWithArgs defaultConfiguration opts site

main = do
  cmd <- execParser (info (mainParser <**> helper) fullDesc)
  case cmd of
    New -> printNewPath
    Build -> runHakyll (Options False Rebuild)
    Serve -> runHakyll (Options False (Watch "localhost" 8000 False))
