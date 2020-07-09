module Types.ProductInfo where

import Beckn.Types.API.Track
import Beckn.Types.Core.Provider
import EulerHS.Prelude
import Control.Lens.Prism (_Just)

data ProductInfo = ProductInfo
  { _provider :: Maybe Provider,
    _tracker :: Maybe Tracker
  }
  deriving (Generic, Show)

instance FromJSON ProductInfo where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProductInfo where
  toJSON = genericToJSON stripAllLensPrefixOptions
