module Beckn.Types.Core.Cabs.OnUpdate.RideOrder.TripAssignedOrder
  ( module Beckn.Types.Core.Cabs.OnUpdate.RideOrder.TripAssignedOrder,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.Common.DecimalValue as Reexport
import Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideOrderStatus (RideOrderStatus (TRIP_ASSIGNED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import Data.Time
import EulerHS.Prelude hiding (id, state, (.=))
import GHC.Exts (fromList)

data TripAssignedOrder = TripAssignedOrder
  { id :: Text,
    fulfillment :: TripAssignedFulfillment
  }
  deriving (Generic, Show)

instance ToJSON TripAssignedOrder where
  toJSON TripAssignedOrder {..} =
    A.Object $
      "id" .= id
        <> "fulfillment" .= fulfillment
        <> "state" .= TRIP_ASSIGNED

instance FromJSON TripAssignedOrder where
  parseJSON = withObject "TripAssignedOrder" $ \obj -> do
    state <- obj .: "state"
    unless (state == TRIP_ASSIGNED) $ fail "Wrong state."
    TripAssignedOrder
      <$> obj .: "id"
      <*> obj .: "fulfillment"

instance ToSchema TripAssignedOrder where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    fulfillment <- declareSchemaRef (Proxy :: Proxy TripAssignedFulfillment)
    updated_at <- declareSchemaRef (Proxy :: Proxy UTCTime)
    state <- declareSchemaRef (Proxy :: Proxy RideOrderStatus)
    return $
      NamedSchema (Just "TripAssignedOrder") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", id),
                ("fulfillment", fulfillment),
                ("updated_at", updated_at),
                ("state", state)
              ]
          & required L..~ ["id", "fulfillment", "updated_at", "state"]

instance Example TripAssignedOrder where
  example =
    TripAssignedOrder
      { id = "ride_booking_id",
        fulfillment = example
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
    rating :: Maybe DecimalValue,
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
  { model :: Text,
    variant :: Text,
    color :: Text,
    registration :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Vehicle where
  example =
    Vehicle
      { model = "model",
        variant = "variant",
        color = "color",
        registration = "regNum"
      }
