{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingReallocationEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingReallocationEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_BOOKING_REALLOCATION))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import GHC.Exts (fromList)
import Kernel.Prelude
import Kernel.Utils.Schema

data BookingReallocationEvent = BookingReallocationEvent
  { id :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo,
    reallocation_reason :: CancellationSource
  }
  deriving (Generic, Show)

instance ToJSON BookingReallocationEvent where
  toJSON BookingReallocationEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "./komn/reallocation_reason" .= reallocation_reason
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_BOOKING_REALLOCATION) :: A.Object)))

instance FromJSON BookingReallocationEvent where
  parseJSON = withObject "BookingReallocationEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "BookingReallocationEvent")
    unless (update_type == RIDE_BOOKING_REALLOCATION) $ fail "Wrong update_type."
    BookingReallocationEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "fulfillment"
      <*> obj .: "./komn/reallocation_reason"

instance ToSchema BookingReallocationEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    reallocationSource <- declareSchemaRef (Proxy :: Proxy CancellationSource)
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
                ("./komn/reallocation_reason", reallocationSource),
                ("fulfillment", Inline fulfillment)
              ]
          & required L..~ ["id", "./komn/update_target", "./komn/reallocation_reason", "fulfillment"]

newtype FulfillmentInfo = FulfillmentInfo
  { id :: Text -- bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
