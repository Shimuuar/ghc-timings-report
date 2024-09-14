{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Monad
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BL
import Options.Applicative
import GhcTimings.Collect


main :: IO ()
main = join
  $ customExecParser (prefs showHelpOnError)
  $ (helper <*> parser)
    `info`
    (  fullDesc
    <> header   "Extract information and generate reports from .dump-timings files"
    <> progDesc
       (unlines
        [ "This program can extract information about build times from .dump-timings"
        , "files produced by GHC and dump in JSON format for later analysis and/or"
        , "generate HTML reports"
        ]))

parser :: Parser (IO ())
parser = do
  subparser $ mconcat
    [ cmd "dump" dumpJSON "Dump timings in JSON format"
    ]
  where
    cmd name fun hlp = command name ((helper <*> fun) `info` header hlp)


----------------------------------------------------------------
-- Concrete commands
----------------------------------------------------------------

dumpJSON :: Parser (IO ())
dumpJSON = do
  dir <- strArgument (metavar "DIR" <> help "Path to project directory")
  pure $ do
    timing <- loadDumpTimings dir
    BL.putStr $ JSON.encode timing
    putStrLn ""
