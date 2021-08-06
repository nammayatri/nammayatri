module Beckn.Types.Mobility.Catalog where

import Beckn.Types.Core.Brand
import Beckn.Types.Core.Category
import Beckn.Types.Core.Item
import Beckn.Types.Core.Model
import Beckn.Types.Core.Offer
import Beckn.Types.Mobility.FareProduct
import Beckn.Utils.Example
import Data.Text
import Data.Time
import EulerHS.Prelude hiding (id)

data Catalog = Catalog
  { id :: Text,
    categories :: [Category],
    brands :: [Brand],
    models :: [Model],
    ttl :: Maybe UTCTime,
    items :: [Item],
    offers :: [Offer],
    -- Mobility specific
    fare_products :: [FareProduct]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Catalog where
  example =
    Catalog
      { id = idExample,
        categories = example,
        brands = example,
        models = example,
        ttl = example,
        items = example,
        offers = example,
        fare_products = example
      }
