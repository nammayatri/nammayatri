{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Update.UpdateEvent.DestinationChangedEvent
  ( module Beckn.Types.Core.Taxi.Update.UpdateEvent.DestinationChangedEvent,
  )
where

-- import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
-- import Beckn.Types.Core.Taxi.Update.UpdateEvent.UpdateEventType (UpdateEventType (DESTINATION_CHANGED))
-- import Beckn.Types.Core.Taxi.Common.FulfillmentInfo as Reexport
import Beckn.Types.Core.Taxi.Common.StartInfo
import Beckn.Types.Core.Taxi.Common.StopInfo
-- import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, title, value)
import EulerHS.Prelude hiding (id)

-- import GHC.Exts (fromList)
-- import Kernel.Utils.Schema

data DestinationChangedEvent = DestinationChangedEvent
  { id :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo
    -- state :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- instance ToSchema DestinationChangedEvent where
--   declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

-- instance FromJSON DestinationChangedEvent where
--   parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

-- instance ToJSON DestinationChangedEvent where
--   toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

-- instance ToJSON DestinationChangedEvent where
--   toJSON DestinationChangedEvent {..} = do
--     let (A.Object fulfJSON) = toJSON fulfillment
--     A.Object $
--       "id" .= id
--         <> "./komn/update_target" .= update_target
--         <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= DESTINATION_CHANGED) :: A.Object)))

-- instance FromJSON DestinationChangedEvent where
--   parseJSON = withObject "DestinationChangedEvent" $ \obj -> do
--     update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
--     unless (update_type == DESTINATION_CHANGED) $ fail "Wrong update_type."
--     DestinationChangedEvent
--       <$> obj .: "id"
--       <*> obj .: "./komn/update_target"
--       <*> obj .: "fulfillment"

-- instance ToSchema DestinationChangedEvent where
--   declareNamedSchema _ = do
--     txt <- declareSchemaRef (Proxy :: Proxy Text)
--     update_type <- declareSchemaRef (Proxy :: Proxy UpdateEventType)
--     let st =
--           mempty
--             & type_ L.?~ OpenApiObject
--             & properties
--               L..~ fromList
--                 [("code", update_type)]
--             & required L..~ ["code"]
--         fulfillment =
--           toInlinedSchema (Proxy :: Proxy FulfillmentInfo)
--             & properties
--               L.<>~ fromList [("state", Inline st)]
--             & required L.<>~ ["state"]
--     return $
--       NamedSchema (Just "DestinationChangedEvent") $
--         mempty
--           & type_ L.?~ OpenApiObject
--           & properties
--             L..~ fromList
--               [ ("id", txt),
--                 ("./komn/update_target", txt),
--                 ("fulfillment", Inline fulfillment)
--               ]
--           & required
--             L..~ [ "id",
--                    "./komn/update_target",
--                    "fulfillment"
--                  ]

data FulfillmentInfo = FulfillmentInfo
  { id :: Text,
    start :: StartInfo,
    end :: StopInfo
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)

-- instance ToSchema FulfillmentInfo where
--   declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

-- instance FromJSON FulfillmentInfo where
--   parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

-- instance ToJSON FulfillmentInfo where
--   toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
