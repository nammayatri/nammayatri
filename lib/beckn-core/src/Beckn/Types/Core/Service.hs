module Beckn.Types.Core.Service where

import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Policy
import Beckn.Types.Core.Provider
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Service = Service
  { _id :: Text,
    _catalog :: Maybe Catalog,
    _provider :: Maybe Provider,
    _policies :: [Policy]
  }
  deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Service where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Service where
  example =
    Service
      { _id = idExample,
        _catalog = example,
        _provider = example,
        _policies = example
      }
