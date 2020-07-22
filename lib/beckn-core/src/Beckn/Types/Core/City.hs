module Beckn.Types.Core.City where

import Data.Text
import EulerHS.Prelude

data City = City
  { _name :: Text,
    _code :: Text
  }
  deriving (Generic, Show)

instance FromJSON City where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON City where
  toJSON = genericToJSON stripAllLensPrefixOptions
