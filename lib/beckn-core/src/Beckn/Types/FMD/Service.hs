module Beckn.Types.FMD.Service where

import Beckn.Types.Core.Brand
import Beckn.Types.Core.Category
import Beckn.Types.Core.Item
import Beckn.Types.Core.Model
import Beckn.Types.Core.Offer
import Beckn.Types.Core.Policy
import Beckn.Types.Core.Provider
import Beckn.Types.FMD.Paradigm
import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude

data Service = Service
  { _id :: Text,
    _provider :: Provider,
    _policies :: [Policy],
    -- FIXME: Catalog field name clashes with the one from Core.Service
    -- We have assumed the domain one here takes precedence
    _catalog :: ServiceCatalog
  }
  deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Service where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ServiceCatalog = ServiceCatalog
  { _id :: Text,
    _categories :: [Category],
    _brands :: [Brand],
    _models :: [Model],
    _ttl :: Maybe UTCTime,
    _items :: [Item],
    _offers :: [Offer],
    _paradigms :: [Paradigm]
  }
  deriving (Generic, Show)

instance FromJSON ServiceCatalog where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ServiceCatalog where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Service where
  example =
    Service
      { _id = idExample,
        _provider = example,
        _policies = example,
        _catalog = example
      }

instance Example ServiceCatalog where
  example =
    ServiceCatalog
      { _id = idExample,
        _categories = example,
        _brands = example,
        _models = example,
        _ttl = example,
        _items = example,
        _offers = example,
        _paradigms = example
      }
