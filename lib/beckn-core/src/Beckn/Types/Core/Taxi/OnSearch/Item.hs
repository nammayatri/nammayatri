module Beckn.Types.Core.Taxi.OnSearch.Item
  ( module Beckn.Types.Core.Taxi.OnSearch.Item,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Price as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.Aeson
import Data.Aeson.Types (parseFail)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

data Item = OneWay OneWayItem | Rental RentalItem deriving (Generic, Show)

instance ToJSON Item where
  toJSON = \case
    Rental rentalItem -> toJSON rentalItem
    OneWay oneWayItem -> toJSON oneWayItem

instance FromJSON Item where
  parseJSON value = withObject "Item" parser value
    where
      parser obj = do
        categoryId <- obj .: ("category_id" :: Text)
        case categoryId :: Text of
          "ONE_WAY" -> OneWay <$> parseJSON value
          "RENTAL" -> Rental <$> parseJSON value
          _ -> parseFail "Invalid category_id"

-- TODO fix ToSchema instance
instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

-- TODO Fix OnSearch Items according to spec. We don't have actual Catalog design now
data OneWayItem = OneWayItem
  { id :: Text,
    category_id :: Text, -- ONE_WAY or RENTALS
    vehicle_variant :: Text,
    estimated_price :: Price,
    discount :: Maybe Price,
    discounted_price :: Price,
    nearest_driver_distance :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalItem = RentalItem
  { id :: Text,
    category_id :: Text, -- ONE_WAY or RENTALS
    vehicle_variant :: Text,
    estimated_price :: Price,
    discount :: Maybe Price,
    discounted_price :: Price,
    baseDistance :: Double,
    baseDurationHr :: Int,
    extraKMFare :: Price,
    extraMinuteFare :: Price,
    driverAllowanceForDay :: Maybe Price
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
