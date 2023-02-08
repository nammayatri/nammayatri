module Beckn.Types.Core.Metro.Search.Scalar (Scalar (..), ScalarRange (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (max, min)
import Kernel.Utils.JSON

data Scalar = Scalar
  { _type :: Maybe ScalarType,
    value :: Int, -- FIXME: probably not integer
    estimated_value :: Maybe Int,
    computed_value :: Maybe Int,
    range :: Maybe ScalarRange,
    unit :: Text
  }
  deriving (Generic, Show, ToSchema)

data ScalarType = CONSTANT | VARIABLE
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data ScalarRange = ScalarRange
  { min :: Int,
    max :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Scalar where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
