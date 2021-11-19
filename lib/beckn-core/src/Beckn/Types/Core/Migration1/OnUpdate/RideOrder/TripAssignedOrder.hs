module Beckn.Types.Core.Migration1.OnUpdate.RideOrder.TripAssignedOrder where

import Beckn.Types.Core.Migration1.OnUpdate.RideOrder.RideOrderStatus (RideOrderStatus (TRIP_ASSIGNED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import Data.Time
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data TripAssignedOrder = TripAssignedOrder
  { id :: Text,
    fulfillment :: TripAssignedFulfillment,
    updated_at :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON TripAssignedOrder where
  toJSON TripAssignedOrder {..} =
    A.Object $
      "id" .= id
        <> "fulfillment" .= fulfillment
        <> "updated_at" .= updated_at
        <> "status" .= TRIP_ASSIGNED

instance FromJSON TripAssignedOrder where
  parseJSON = withObject "TripAssignedOrder" $ \obj -> do
    status <- obj .: "status"
    when (status == TRIP_ASSIGNED) $ fail "Wrong status."
    TripAssignedOrder
      <$> obj .: "id"
      <*> obj .: "fulfillment"
      <*> obj .: "updated_at"

instance ToSchema TripAssignedOrder where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    fulfillment <- declareSchemaRef (Proxy :: Proxy TripAssignedFulfillment)
    updated_at <- declareSchemaRef (Proxy :: Proxy UTCTime)
    status <- declareSchemaRef (Proxy :: Proxy RideOrderStatus)
    return $
      NamedSchema (Just "TripAssignedOrder") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", id),
                ("fulfillment", fulfillment),
                ("updated_at", updated_at),
                ("status", status)
              ]
          & required L..~ ["id", "fulfillment", "updated_at", "status"]

instance Example TripAssignedOrder where
  example =
    TripAssignedOrder
      { id = "ride_booking_id",
        fulfillment = example,
        updated_at = example
      }

data TripAssignedFulfillment = TripAssignedFulfillment
  { id :: Text,
    agent :: Agent,
    vehicle :: Vehicle,
    otp :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example TripAssignedFulfillment where
  example =
    TripAssignedFulfillment
      { id = "ride_id",
        agent = example,
        vehicle = example,
        otp = "1234"
      }

data Agent = Agent
  { name :: Text,
    phone :: Text,
    rating :: Maybe Double,
    registered_at :: UTCTime
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example Agent where
  example =
    Agent
      { name = "Bob Ross",
        phone = "+91899999999",
        rating = Just 3.4,
        registered_at = example
      }

data Vehicle = Vehicle
  { make :: Maybe Text,
    model :: Maybe Text,
    variant :: Maybe Text,
    color :: Maybe Text,
    registration :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Vehicle where
  example =
    Vehicle
      { make = Nothing,
        model = Nothing,
        variant = Nothing,
        color = Nothing,
        registration = Nothing
      }
