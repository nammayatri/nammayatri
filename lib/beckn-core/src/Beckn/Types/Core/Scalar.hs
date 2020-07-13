module Beckn.Types.Core.Scalar where

import Beckn.Types.Core.Api
import Beckn.Types.Core.Contact
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Scalar = Scalar
  { _value :: Double,
    _unit :: Text
  }
  deriving (Generic, Show)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Scalar where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Scalar where
  example =
    Scalar
      { _value = 12.345,
        _unit = "meters"
      }
