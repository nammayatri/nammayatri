module Beckn.Types.Core.Migration.Scalar (Scalar (..), Range (..)) where

import Beckn.Utils.JSON
import EulerHS.Prelude hiding (max, min)

data Scalar = Scalar
  { _type :: Maybe ScalarType,
    value :: Int, -- FIXME: probably not integer
    estimated_value :: Maybe Int,
    computed_value :: Maybe Int,
    range :: Maybe Range,
    unit :: Text
  }
  deriving (Generic, Show)

data ScalarType = CONSTANT | VARIABLE
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data Range = Range
  { min :: Int,
    max :: Int
  }
  deriving (Generic, Show)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Scalar where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Range where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Range where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
