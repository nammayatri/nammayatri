module Types.Beckn.Service where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude hiding (id)
import Types.Beckn.Brand
import Types.Beckn.Category
import Types.Beckn.Item
import Types.Beckn.Model
import Types.Beckn.Offer
import Types.Beckn.Paradigm
import Types.Beckn.Policy
import Types.Beckn.Provider

data Service = Service
  { id :: Text,
    provider :: Provider,
    policies :: [Policy],
    -- FIXME: Catalog field name clashes with the one from Core.Service
    -- We have assumed the domain one here takes precedence
    catalog :: ServiceCatalog
  }
  deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Service where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ServiceCatalog = ServiceCatalog
  { id :: Text,
    categories :: [Category],
    brands :: [Brand],
    models :: [Model],
    ttl :: Maybe UTCTime,
    items :: [Item],
    offers :: [Offer],
    paradigms :: [Paradigm]
  }
  deriving (Generic, Show)

instance FromJSON ServiceCatalog where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ServiceCatalog where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Service where
  example =
    Service
      { id = idExample,
        provider = example,
        policies = example,
        catalog = example
      }

instance Example ServiceCatalog where
  example =
    ServiceCatalog
      { id = idExample,
        categories = example,
        brands = example,
        models = example,
        ttl = example,
        items = example,
        offers = example,
        paradigms = example
      }
