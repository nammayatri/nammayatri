module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_STARTED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (id, state, (.=))
import GHC.Exts (fromList)

data RideStartedEvent = RideStartedEvent
  { order_id :: Text,
    fulfillment_id :: Text
  }
  deriving (Generic, Show)

instance ToJSON RideStartedEvent where
  toJSON RideStartedEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "fulfillment_id" .= fulfillment_id
        <> "state" .= RIDE_STARTED

instance FromJSON RideStartedEvent where
  parseJSON = withObject "RideStartedEvent" $ \obj -> do
    state <- obj .: "state"
    unless (state == RIDE_STARTED) $ fail "Wrong state."
    RideStartedEvent
      <$> obj .: "order_id"
      <*> obj .: "fulfillment_id"

instance ToSchema RideStartedEvent where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    state <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "RideStartedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", id),
                ("fulfillment_id", id),
                ("state", state)
              ]
          & required L..~ ["order_id", "fulfillment_id", "state"]

instance Example RideStartedEvent where
  example =
    RideStartedEvent
      { order_id = "ride_booking_id",
        fulfillment_id = "ride_id"
      }