{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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
import Data.List         (stripPrefix)
import Data.Version
import Data.Map.Strict   qualified as Map
import System.FilePath
import System.Directory
import Streaming.Prelude qualified as S
import Streaming.Prelude (Stream,Of)
import Text.ParserCombinators.ReadP (readP_to_S,eof,satisfy,char)


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
  $ walkDir  (\_    -> iterGHC)
  $ walkDir  (\_ () -> S.yield)
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
  liftIO $ print dir
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
  


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data Dir a = Dir FilePath a
  deriving stock Show



walkDir :: (FilePath -> a -> FilePath -> Stream (Of b) IO ())
        -> Stream (Of (Dir a)) IO ()
        -> Stream (Of (Dir b)) IO ()
walkDir fun stream = S.for stream $ \(Dir dir a) -> do
  paths <- liftIO $ listDirectory dir
  for_ paths $ \path ->
    S.map (\b -> Dir (dir</>path) b) (fun dir a path)

walkDir_ :: (FilePath -> a -> FilePath -> Stream (Of b) IO ())
         -> Stream (Of (Dir a)) IO ()
         -> Stream (Of b) IO ()
walkDir_ fun stream = S.for stream $ \(Dir dir a) -> do
  paths <- liftIO $ listDirectory dir
  for_ paths $ fun dir a
