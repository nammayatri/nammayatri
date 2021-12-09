module Beckn.Types.Core.Cabs.OnUpdate.OnUpdateEvent.RideCompletedEvent
  ( module Beckn.Types.Core.Cabs.OnUpdate.OnUpdateEvent.RideCompletedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.Common.Payment as Reexport
import Beckn.Types.Core.Cabs.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_COMPLETED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (id, state, (.=))
import GHC.Exts (fromList)

data RideCompletedEvent = RideCompletedEvent
  { order_id :: Text,
    fulfillment_id :: Text,
    chargeable_distance :: DecimalValue,
    payment :: Payment
  }
  deriving (Generic, Show)

instance ToJSON RideCompletedEvent where
  toJSON RideCompletedEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "fulfillment_id" .= fulfillment_id
        <> "chargeable_distance" .= chargeable_distance
        <> "payment" .= payment
        <> "state" .= RIDE_COMPLETED

instance FromJSON RideCompletedEvent where
  parseJSON = withObject "RideCompletedEvent" $ \obj -> do
    state <- obj .: "state"
    unless (state == RIDE_COMPLETED) $ fail "Wrong state."
    RideCompletedEvent
      <$> obj .: "order_id"
      <*> obj .: "fulfillment_id"
      <*> obj .: "chargeable_distance"
      <*> obj .: "payment"

instance ToSchema RideCompletedEvent where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    decimalValue <- declareSchemaRef (Proxy :: Proxy DecimalValue)
    payment <- declareSchemaRef (Proxy :: Proxy Payment)
    state <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "RideCompletedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", id),
                ("fulfillment_id", id),
                ("chargeable_distance", decimalValue),
                ("payment", payment),
                ("state", state)
              ]
          & required
            L..~ [ "order_id",
                   "fulfillment_id",
                   "chargeable_distance",
                   "payment",
                   "state"
                 ]

instance Example RideCompletedEvent where
  example =
    RideCompletedEvent
      { order_id = "ride_booking_id",
        fulfillment_id = "ride_id",
        chargeable_distance = 123,
        payment = example
      }
