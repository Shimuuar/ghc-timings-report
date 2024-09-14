{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Monad
import Data.Aeson               qualified as JSON
import Data.ByteString.Lazy     qualified as BL
import Data.Map.Strict          (Map)
import Data.Text                (Text)
import Data.Foldable
import Options.Applicative
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Blaze               (Markup)
import System.Directory         (createDirectoryIfMissing,copyFile)
import System.FilePath          ((</>),(<.>))

import GhcTimings.Collect
import GhcTimings.Types
import GhcTimings.Report  qualified as Report

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
    [ cmd "dump"     dumpJSON  "Dump timings in JSON format"
    , cmd "generate" genReport "Generate HTML report"
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

genReport :: Parser (IO ())
genReport = do
  out <- strArgument (metavar "DIR" <> help "Output directory")
  js  <- strArgument mempty
  pure $ do
    bs   <- BL.readFile js
    pkgs :: [Package ([Phase], Map Text [Phase])] <- case JSON.decode bs of
      Nothing   -> error "Cannot decode"
      Just pkgs -> pure pkgs
    --
    createDirectoryIfMissing True out
    mkHtmlFile (out </> "index.html") $ Report.index pkgs
    for_ pkgs $ \pkg ->
      mkHtmlFile (out </> pkg.name <.> "html") $ Report.package pkg
    copyFile "files/main.css" (out </> "main.css") -- TODO use data files

mkHtmlFile :: FilePath -> Markup -> IO ()
mkHtmlFile nm markup = do
  BL.writeFile nm $ renderMarkup markup
