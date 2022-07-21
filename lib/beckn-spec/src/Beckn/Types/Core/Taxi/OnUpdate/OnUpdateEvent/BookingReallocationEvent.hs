module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingReallocationEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingReallocationEvent,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_BOOKING_REALLOCATION))
import Beckn.Utils.Schema
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import GHC.Exts (fromList)

data BookingReallocationEvent = BookingReallocationEvent
  { id :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON BookingReallocationEvent where
  toJSON BookingReallocationEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_BOOKING_REALLOCATION) :: A.Object)))

instance FromJSON BookingReallocationEvent where
  parseJSON = withObject "BookingReallocationEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_BOOKING_REALLOCATION) $ fail "Wrong update_type."
    BookingReallocationEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "fulfillment"

instance ToSchema BookingReallocationEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    let st =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("code", update_type)]
            & required L..~ ["code"]
        fulfillment =
          toInlinedSchema (Proxy :: Proxy FulfillmentInfo)
            & properties
              L.<>~ fromList [("state", Inline st)]
            & required L.<>~ ["state"]
    return $
      NamedSchema (Just "BookingReallocationEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("fulfillment", Inline fulfillment)
              ]
          & required L..~ ["id", "./komn/update_target", "fulfillment"]

newtype FulfillmentInfo = FulfillmentInfo
  { id :: Text -- bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
