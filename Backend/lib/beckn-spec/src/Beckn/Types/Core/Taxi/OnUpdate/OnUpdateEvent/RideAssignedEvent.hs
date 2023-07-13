{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Agent as Reexport
import Beckn.Types.Core.Taxi.Common.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Common.Vehicle as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_ASSIGNED))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name, tags)
import GHC.Exts (fromList)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data RideAssignedEvent = RideAssignedEvent
  { id :: Text,
    state :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON RideAssignedEvent where
  toJSON RideAssignedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "state" .= state
        <> "./komn/update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_ASSIGNED) :: A.Object)))

instance FromJSON RideAssignedEvent where
  parseJSON = withObject "RideAssignedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_ASSIGNED) $ fail "Wrong update_type."
    RideAssignedEvent
      <$> obj .: "id"
      <*> obj .: "state"
      <*> obj .: "./komn/update_target"
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
                [("code", update_type)]
            & required L..~ ["code"]
        fulfillment =
          toInlinedSchema (Proxy :: Proxy FulfillmentInfo)
            & properties
              L.<>~ fromList [("state", Inline st)]
            & required L.<>~ ["state"]
    return $
      NamedSchema (Just "RideAssignedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("state", txt),
                ("./komn/update_target", txt),
                ("fulfillment", Inline fulfillment)
              ]
          & required
            L..~ [ "id",
                   "state",
                   "./komn/update_target",
                   "fulfillment"
                 ]

data FulfillmentInfo = FulfillmentInfo
  { id :: Text, -- bppRideId
    start :: StartInfo,
    agent :: Agent,
    vehicle :: Vehicle
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
