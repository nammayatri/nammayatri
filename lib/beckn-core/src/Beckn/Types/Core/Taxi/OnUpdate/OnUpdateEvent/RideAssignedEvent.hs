module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_ASSIGNED))
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import Data.Time
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data RideAssignedEvent = RideAssignedEvent
  { order_id :: Text,
    ride_id :: Text,
    agent :: Agent,
    vehicle :: Vehicle,
    otp :: Text
  }
  deriving (Generic, Show)

instance ToJSON RideAssignedEvent where
  toJSON RideAssignedEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "ride_id" .= ride_id
        <> "agent" .= agent
        <> "vehicle" .= vehicle
        <> "otp" .= otp
        <> "update_type" .= RIDE_ASSIGNED

instance FromJSON RideAssignedEvent where
  parseJSON = withObject "RideAssignedEvent" $ \obj -> do
    update_type <- obj .: "update_type"
    unless (update_type == RIDE_ASSIGNED) $ fail "Wrong update_type."
    RideAssignedEvent
      <$> obj .: "order_id"
      <*> obj .: "ride_id"
      <*> obj .: "agent"
      <*> obj .: "vehicle"
      <*> obj .: "otp"

instance ToSchema RideAssignedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    agent <- declareSchemaRef (Proxy :: Proxy Agent)
    vehicle <- declareSchemaRef (Proxy :: Proxy Vehicle)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "RideAssignedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", txt),
                ("ride_id", txt),
                ("agent", agent),
                ("vehicle", vehicle),
                ("otp", txt),
                ("update_type", update_type)
              ]
          & required
            L..~ [ "order_id",
                   "ride_id",
                   "agent",
                   "vehicle",
                   "otp",
                   "update_type"
                 ]

instance Example RideAssignedEvent where
  example =
    RideAssignedEvent
      { order_id = "ride_booking_id",
        ride_id = "ride_id",
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
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ToSchema Agent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

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
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Vehicle where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Vehicle where
  example =
    Vehicle
      { model = "model",
        variant = "variant",
        color = "color",
        registration = "regNum"
      }
