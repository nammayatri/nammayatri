{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_BOOKING_CANCELLED))
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.Tags
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name, tags)
import GHC.Exts (fromList)
import Kernel.Prelude
import Kernel.Utils.Schema

data BookingCancelledEvent = BookingCancelledEvent
  { id :: Text,
    update_target :: Text,
    state :: Text,
    cancellation_reason :: CancellationSource,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON BookingCancelledEvent where
  toJSON BookingCancelledEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "state" .= state
        <> "./komn/cancellation_reason" .= cancellation_reason
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_BOOKING_CANCELLED) :: A.Object)))

instance FromJSON BookingCancelledEvent where
  parseJSON = withObject "BookingCancelledEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_BOOKING_CANCELLED) $ fail "Wrong update_type."
    BookingCancelledEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "state"
      <*> obj .: "./komn/cancellation_reason"
      <*> obj .: "fulfillment"

instance ToSchema BookingCancelledEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    cancellationSource <- declareSchemaRef (Proxy :: Proxy CancellationSource)
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
      NamedSchema (Just "BookingCancelledEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("state", txt),
                ("./komn/cancellation_reason", cancellationSource),
                ("fulfillment", Inline fulfillment)
              ]
          & required
            L..~ [ "id",
                   "./komn/update_target",
                   "state",
                   "./komn/cancellation_reason",
                   "fulfillment"
                 ]

newtype FulfillmentInfo = FulfillmentInfo
  { tags :: Maybe Tags
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
