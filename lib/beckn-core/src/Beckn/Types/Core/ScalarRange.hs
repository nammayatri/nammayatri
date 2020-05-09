module Beckn.Types.Core.ScalarRange where

import Beckn.Types.Core.Api
import Beckn.Types.Core.Contact
import Data.Text
import EulerHS.Prelude

data ScalarRange = ScalarRange
  { _min :: Double,
    _max :: Double,
    _unit :: Text
  }
  deriving (Generic, Show)

instance FromJSON ScalarRange where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ScalarRange where
  toJSON = genericToJSON stripAllLensPrefixOptions
