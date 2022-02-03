module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingReallocationEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingReallocationEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_BOOKING_REALLOCATION))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data RideBookingReallocationEvent = RideBookingReallocationEvent
  { order_id :: Text,
    ride_id :: Text,
    cancellation_reason_id :: CancellationSource
  }
  deriving (Generic, Show)

instance ToJSON RideBookingReallocationEvent where
  toJSON RideBookingReallocationEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "ride_id" .= ride_id
        <> "cancellation_reason_id" .= cancellation_reason_id
        <> "update_type" .= RIDE_BOOKING_REALLOCATION

instance FromJSON RideBookingReallocationEvent where
  parseJSON = withObject "RideBookingReallocationEvent" $ \obj -> do
    update_type <- obj .: "update_type"
    unless (update_type == RIDE_BOOKING_REALLOCATION) $ fail "Wrong update_type."
    RideBookingReallocationEvent
      <$> obj .: "order_id"
      <*> obj .: "ride_id"
      <*> obj .: "cancellation_reason_id"

instance ToSchema RideBookingReallocationEvent where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    cancellationSource <- declareSchemaRef (Proxy :: Proxy CancellationSource)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "RideBookingReallocationEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", id),
                ("ride_id", id),
                ("cancellation_reason_id", cancellationSource),
                ("update_type", update_type)
              ]
          & required
            L..~ [ "order_id",
                   "ride_id",
                   "cancellation_reason_id",
                   "update_type"
                 ]

instance Example RideBookingReallocationEvent where
  example =
    RideBookingReallocationEvent
      { order_id = "ride_booking_id",
        ride_id = "ride_id",
        cancellation_reason_id = ByDriver
      }
