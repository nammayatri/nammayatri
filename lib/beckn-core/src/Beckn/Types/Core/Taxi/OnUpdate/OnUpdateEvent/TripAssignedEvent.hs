module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.TripAssignedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.TripAssignedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (TRIP_ASSIGNED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import Data.Time
import EulerHS.Prelude hiding (id, state, (.=))
import GHC.Exts (fromList)

data TripAssignedEvent = TripAssignedEvent
  { order_id :: Text,
    fulfillment_id :: Text,
    agent :: Agent,
    vehicle :: Vehicle,
    otp :: Text
  }
  deriving (Generic, Show)

instance ToJSON TripAssignedEvent where
  toJSON TripAssignedEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "fulfillment_id" .= fulfillment_id
        <> "agent" .= agent
        <> "vehicle" .= vehicle
        <> "otp" .= otp
        <> "state" .= TRIP_ASSIGNED

instance FromJSON TripAssignedEvent where
  parseJSON = withObject "TripAssignedEvent" $ \obj -> do
    state <- obj .: "state"
    unless (state == TRIP_ASSIGNED) $ fail "Wrong state."
    TripAssignedEvent
      <$> obj .: "order_id"
      <*> obj .: "fulfillment_id"
      <*> obj .: "agent"
      <*> obj .: "vehicle"
      <*> obj .: "otp"

instance ToSchema TripAssignedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    agent <- declareSchemaRef (Proxy :: Proxy Agent)
    vehicle <- declareSchemaRef (Proxy :: Proxy Vehicle)
    state <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "TripAssignedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", txt),
                ("fulfillment_id", txt),
                ("agent", agent),
                ("vehicle", vehicle),
                ("otp", txt),
                ("state", state)
              ]
          & required
            L..~ [ "order_id",
                   "fulfillment_id",
                   "agent",
                   "vehicle",
                   "otp",
                   "state"
                 ]

instance Example TripAssignedEvent where
  example =
    TripAssignedEvent
      { order_id = "ride_booking_id",
        fulfillment_id = "ride_id",
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
