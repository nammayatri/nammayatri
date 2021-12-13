module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_STARTED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data RideStartedEvent = RideStartedEvent
  { order_id :: Text,
    ride_id :: Text
  }
  deriving (Generic, Show)

instance ToJSON RideStartedEvent where
  toJSON RideStartedEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "ride_id" .= ride_id
        <> "update_type" .= RIDE_STARTED

instance FromJSON RideStartedEvent where
  parseJSON = withObject "RideStartedEvent" $ \obj -> do
    update_type <- obj .: "update_type"
    unless (update_type == RIDE_STARTED) $ fail "Wrong update_type."
    RideStartedEvent
      <$> obj .: "order_id"
      <*> obj .: "ride_id"

instance ToSchema RideStartedEvent where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "RideStartedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", id),
                ("ride_id", id),
                ("update_type", update_type)
              ]
          & required L..~ ["order_id", "ride_id", "update_type"]

instance Example RideStartedEvent where
  example =
    RideStartedEvent
      { order_id = "ride_booking_id",
        ride_id = "ride_id"
      }