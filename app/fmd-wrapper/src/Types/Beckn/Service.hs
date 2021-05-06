module Types.Beckn.Service where

import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude
import Types.Beckn.Brand
import Types.Beckn.Category
import Types.Beckn.Item
import Types.Beckn.Model
import Types.Beckn.Offer
import Types.Beckn.Paradigm
import Types.Beckn.Policy
import Types.Beckn.Provider

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
