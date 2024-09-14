{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Collection of 
module GhcTimings.Collect
  ( findPackages
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception
import Data.Foldable
import Data.Char
import Data.Function
import Data.List         (stripPrefix)
import Data.Version
import Data.Maybe
import Data.Map.Strict   qualified as Map
import Data.Text.IO      qualified as T
import Data.Text         qualified as T
import System.FilePath
import System.Directory
import Streaming.Prelude qualified as S
import Streaming.Prelude (Stream,Of)
import Text.ParserCombinators.ReadP (readP_to_S,eof,satisfy,char)
import Text.Regex.Applicative

-- import Control.Lens
-- import Control.Lens.Regex.Text qualified as RE

import GhcTimings.Types


----------------------------------------------------------------
-- Find packages
----------------------------------------------------------------

-- | Find all packages present in the tree. Return package where each
--   submodule contains information about 
findPackages 
  :: FilePath -- ^ Path to @dist-newstyle@ directory
  -> IO [Package FilePath]
findPackages dir
  = S.toList_
  $ walkDir_ iterPkg
  $ walkDir  iterGHC
  $ walkDir  (\() -> S.yield)
  $ S.yield (Dir (dir </> "dist-newstyle" </> "build") ())

-- Parse version of GHC
iterGHC :: String -> FilePath -> Stream (Of (String,Version)) IO ()
iterGHC hostOs path
  | Just sver  <- stripPrefix "ghc-" path
  , [(ghc,"")] <- readP_to_S (parseVersion <* eof) sver
    = S.yield (hostOs, ghc)
  | otherwise
    = pure ()

-- Parse version of package
iterPkg :: FilePath -> (String, Version) -> FilePath -> Stream (Of (Package FilePath)) IO ()
iterPkg dir (hostOs, ghc) path
  | [((name,version),"")] <- pkgParser path
    = do components <- liftIO $ Map.fromList <$> listComponents (dir </> path)
         S.yield Package{..}
  | otherwise
    = error $ "Invalid package name: " ++ path
  where
    pkgParser = readP_to_S $ do
      p <- many (satisfy (\_->True)) <* char '-'
      v <- parseVersion <* eof
      pure (p,v)

listComponents :: FilePath -> IO [(CompName, FilePath)]
listComponents dir = S.toList_ $ do
  S.yield (MainLib, dir </> "build")
  comp SubLib "l"
  comp Exe    "x"
  comp Test   "t"
  comp Bench  "b"
  where
    comp mk path = do
      names <- liftIO $ listDirectory (dir </> path) `catch` \(e::IOException) -> pure []
      for_ names $ \nm -> S.yield (mk nm, dir </> path </> nm)

----------------------------------------------------------------
-- Find and parse *.dump_timings files
----------------------------------------------------------------

findDumpTimings :: FilePath -> Stream (Of FilePath) IO ()
findDumpTimings dir = do
  paths <- liftIO $ listDirectory dir
  for_ paths $ \nm -> do
    let path = dir </> nm
    liftIO (doesDirectoryExist path) >>= \case
      True  -> findDumpTimings path
      False
        | (_,".dump-timings") <- splitExtension nm
          -> S.yield path
        | otherwise
          -> pure ()

parseDumpTiming :: Stream (Of FilePath) IO ()
                -> Stream (Of (FilePath, [Phase])) IO ()
parseDumpTiming paths = S.for paths $ \p -> do
  timing <- liftIO $ parsePhases <$> T.readFile p
  S.yield (p, timing)

-- | Parse .dump-timing file produced by GHC. This is very ad-hoc
--   regexp based parser. Hopefully it will parse files correctly
parsePhases :: T.Text -> [Phase]
parsePhases
  = map parseStep
  . filter (not . T.null)
  . T.lines
  where
    parseStep str = case parser str of
      Just a  -> a
      Nothing -> error $ "illegal line: '" <> T.unpack str <> "'"
    parser str =  (T.unpack str =~ rePhaseModule)
              <|> (T.unpack str =~ rePhaseNoModule)


rePhaseModule :: RE Char Phase
rePhaseModule = do
  name <- few $ psym (`notElem` ("[]"::String))
  mod  <- skipWS *> sym '['
       *> (  Just . T.pack <$> some (psym (\c -> isAlphaNum c || c == '.'))
         <|> Nothing       <$  some (psym (`notElem` ("[]"::String)))
          )
       <* string "]:"
  (alloc,time) <- reAllocTime
  pure Phase{ name  = T.pack name
            , ..
            }

rePhaseNoModule :: RE Char Phase
rePhaseNoModule = do
  name <- few $ psym (`notElem` ("[]"::String))
  skipWS *> sym ':'
  (alloc,time) <- reAllocTime
  pure Phase{ name  = T.pack name
            , mod   = Nothing
            , ..
            }

reAllocTime :: RE Char (Int,Double)
reAllocTime = do
  alloc <- skipWS *> string "alloc=" *> some (psym isDigit)
  time  <- skipWS *> string "time=" *> some (psym (\c -> isDigit c || c=='.'))
  pure (read alloc, read time)

skipWS :: RE Char ()
skipWS = void $ many (psym isSpace)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Directory path and value associated to this directory.
data Dir a = Dir FilePath a
  deriving stock Show



walkDir
  :: (a -> FilePath -> Stream (Of b) IO ())
     -- ^ Callback 
  -> Stream (Of (Dir a)) IO ()
  -> Stream (Of (Dir b)) IO ()
walkDir fun stream = S.for stream $ \(Dir dir a) -> do
  paths <- liftIO $ listDirectory dir
  for_ paths $ \path ->
    S.map (\b -> Dir (dir</>path) b) (fun a path)

walkDir_ :: (FilePath -> a -> FilePath -> Stream (Of b) IO ())
         -> Stream (Of (Dir a)) IO ()
         -> Stream (Of b) IO ()
walkDir_ fun stream = S.for stream $ \(Dir dir a) -> do
  paths <- liftIO $ listDirectory dir
  for_ paths $ fun dir a
