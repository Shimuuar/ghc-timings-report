-- |
module GhcTimings.Types where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Csv        (ToNamedRecord,FromNamedRecord)
import Data.Text       (Text)
import Data.Version
import Data.Text       qualified as T
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import GHC.Generics    (Generic)


-- | Name of component in package
data CompName
  = MainLib          -- ^ Main library
  | SubLib !FilePath -- ^ Named sublibrary
  | Exe    !FilePath -- ^ Executable
  | Test   !FilePath -- ^ Test executable
  | Bench  !FilePath -- ^ Benchmark executable
  deriving stock    (Generic, Eq, Ord, Show)


-- | Information about package
data Package a = Package
  { hostOs     :: String         -- ^ Operating system and target CPU
  , ghc        :: Version        -- ^ GHC version
  , name       :: String         -- ^ Name of package
  , version    :: Version        -- ^ Version of package
  , components :: Map CompName a -- ^ All components of a package
  }
  deriving stock    (Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving anyclass (ToJSON, FromJSON)

-- | Infomation about component of a package
data Component = Component
  { modules   :: Map Text [Phase]
    -- ^ Information about modules
  , nonModule :: [Phase]
    -- ^ Timing information nor associated with modules
  }
  deriving stock    (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Single build phase that is reported in the ghc timings output.
data Phase = Phase
  { name   :: !Text         -- ^ Name of phase
  , mod    :: !(Maybe Text) -- ^ Module information
  , alloc  :: !Int          -- ^ Allocation amount
  , time   :: !Double       -- ^ Time spent
  }
  deriving stock    (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)
  deriving anyclass (ToNamedRecord, FromNamedRecord)


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance FromJSON CompName where
  parseJSON = parseCompName <=< parseJSON
instance ToJSON CompName where
  toJSON = toJSON . encodeCompName
instance FromJSONKey CompName where
  fromJSONKey = FromJSONKeyTextParser parseCompName
instance ToJSONKey   CompName where
  toJSONKey = toJSONKeyText (T.pack . encodeCompName)

encodeCompName :: CompName -> String
encodeCompName = \case
  SubLib s -> 'l':'/':s
  Exe    s -> 'x':'/':s
  Test   s -> 't':'/':s
  Bench  s -> 'b':'/':s
  MainLib  -> "build"

parseCompName :: Text -> Parser CompName
parseCompName str = case T.unpack str of
  'l':'/':s -> pure $ SubLib s
  'x':'/':s -> pure $ Exe    s
  't':'/':s -> pure $ Test   s
  'b':'/':s -> pure $ Bench  s
  "build"   -> pure MainLib
  s         -> fail $ "Incorect CompName:" ++ s
