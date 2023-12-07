{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.FulfillmentInfo as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_ASSIGNED))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name, tags)
import GHC.Exts (fromList)
import Kernel.Prelude

data RideAssignedEventV2 = RideAssignedEventV2
  { id :: Text,
    state :: Text,
    -- update_target :: Text,
    fulfillment :: FulfillmentInfoV2
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data RideAssignedEvent = RideAssignedEvent
  { id :: Text,
    state :: Text,
    -- update_target :: Text,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON RideAssignedEvent where
  toJSON RideAssignedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "state" .= state
        -- <> "update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= ("descriptor" .= (("code" .= RIDE_ASSIGNED <> "name" .= A.String "Ride Assigned") :: A.Object) :: A.Object)))

instance FromJSON RideAssignedEvent where
  parseJSON = withObject "RideAssignedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "descriptor") >>= (.: "code")
    unless (update_type == RIDE_ASSIGNED) $ fail "Wrong update_type."
    RideAssignedEvent
      <$> obj .: "id"
      <*> obj .: "state"
      -- <*> obj .: "update_target"
      <*> obj .: "fulfillment"

instance ToSchema RideAssignedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    let st =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("code", update_type), ("name", txt)]
            & required L..~ ["code", "name"]
        descriptor =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("descriptor", Inline st)]
            & required L..~ ["descriptor"]
        fulfillment =
          toInlinedSchema (Proxy :: Proxy FulfillmentInfo)
            & properties
              L.<>~ fromList [("state", Inline descriptor)]
            & required L.<>~ ["state"]
    return $
      NamedSchema (Just "RideAssignedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("state", txt),
                -- ("update_target", txt),
                ("fulfillment", Inline fulfillment)
              ]
          & required
            L..~ [ "id",
                   "state",
                   --  "update_target",
                   "fulfillment"
                 ]
