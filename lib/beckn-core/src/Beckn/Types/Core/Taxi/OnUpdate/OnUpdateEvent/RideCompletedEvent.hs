module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Price as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_COMPLETED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data RideCompletedEvent = RideCompletedEvent
  { order_id :: Text,
    ride_id :: Text,
    chargeable_distance :: DecimalValue,
    fare :: Price,
    total_fare :: Price
  }
  deriving (Generic, Show)

instance ToJSON RideCompletedEvent where
  toJSON RideCompletedEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "ride_id" .= ride_id
        <> "chargeable_distance" .= chargeable_distance
        <> "fare" .= fare
        <> "total_fare" .= total_fare
        <> "update_type" .= RIDE_COMPLETED

instance FromJSON RideCompletedEvent where
  parseJSON = withObject "RideCompletedEvent" $ \obj -> do
    update_type <- obj .: "update_type"
    unless (update_type == RIDE_COMPLETED) $ fail "Wrong update_type."
    RideCompletedEvent
      <$> obj .: "order_id"
      <*> obj .: "ride_id"
      <*> obj .: "chargeable_distance"
      <*> obj .: "fare"
      <*> obj .: "total_fare"

instance ToSchema RideCompletedEvent where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    decimalValue <- declareSchemaRef (Proxy :: Proxy DecimalValue)
    price <- declareSchemaRef (Proxy :: Proxy Price)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "RideCompletedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", id),
                ("ride_id", id),
                ("chargeable_distance", decimalValue),
                ("total_fare", price),
                ("fare", price),
                ("update_type", update_type)
              ]
          & required
            L..~ [ "order_id",
                   "ride_id",
                   "chargeable_distance",
                   "total_fare",
                   "fare",
                   "update_type"
                 ]

instance Example RideCompletedEvent where
  example =
    RideCompletedEvent
      { order_id = "ride_booking_id",
        ride_id = "ride_id",
        chargeable_distance = 123,
        total_fare = example,
        fare = example
      }
