-- module Beckn.Types.Core.Migration.Time where
 module Core.Time where

-- import Beckn.Types.Core.Migration.Duration (Duration)
import Data.Time (UTCTime)
-- import Beckn.Utils.JSON
-- import EulerHS.Prelude
import Data.Aeson
import Data.Text
import GHC.Generics

data Time = Time
  { label :: Maybe Text,
    timestamp :: Maybe UTCTime
    -- duration :: Maybe Duration,
    -- range :: Maybe Range,
    -- days :: Maybe Text
  }
  deriving (Generic, FromJSON, Show, Eq)

instance ToJSON Time where
  toJSON = genericToJSON  defaultOptions { omitNothingFields = True }


emptyTime :: Time
emptyTime =
  Time
    { label = Nothing,
      timestamp = Nothing
    --   duration = Nothing,
    --   range = Nothing,
    --   days = Nothing
    }

-- data Range = Range
--   { start :: UTCTime,
    -- end :: UTCTime
--   }
--   deriving (Generic, FromJSON, ToJSON, Show, Eq)
-- 