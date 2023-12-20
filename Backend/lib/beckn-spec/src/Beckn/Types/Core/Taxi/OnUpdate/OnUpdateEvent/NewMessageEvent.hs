{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.NewMessageEvent where

import Beckn.Types.Core.Taxi.Common.FulfillmentInfo
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (NEW_MESSAGE))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (fromList, id)
import GHC.Exts (fromList)
import Kernel.Utils.JSON
import Kernel.Utils.Schema

data NewMessageEventV2 = NewMessageEventV2
  { id :: Text,
    -- update_target :: Text,
    fulfillments :: [FulfillmentInfoV2]
  }
  deriving (Generic, Show)

instance ToJSON NewMessageEventV2 where
  toJSON = genericToJSON removeNullFields

-- toJSON NewMessageEventV2 {..} = do
--   let (A.Object fulfJSON) = toJSON fulfillments
--   A.Object $
--     "id" .= id
--       -- <> "update_target" .= update_target
--       <> "fulfillments" .= (fulfJSON <> ("status" .= ("descriptor" .= (("code" .= NEW_MESSAGE <> "name" .= A.String "New Message") :: A.Object) :: A.Object)))

instance FromJSON NewMessageEventV2 where
  parseJSON = genericParseJSON removeNullFields

-- parseJSON = withObject "NewMessageEventV2" $ \obj -> do
--   update_type <- (obj .: "fulfillments") >>= (.: "status") >>= (.: "descriptor") >>= (.: "code")
--   unless (update_type == NEW_MESSAGE) $ fail "Wrong update_type."
--   NewMessageEventV2
--     <$> obj .: "id"
--     -- <*> obj .: "update_target"
--     <*> obj .: "fulfillments"

instance ToSchema NewMessageEventV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

-- declareNamedSchema _ = do
--   txt <- declareSchemaRef (Proxy :: Proxy Text)
--   update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
--   let st =
--         mempty
--           & type_ L.?~ OpenApiObject
--           & properties
--             L..~ fromList
--               [("code", update_type), ("name", txt)]
--           & required L..~ ["code", "name"]
--       descriptor =
--         mempty
--           & type_ L.?~ OpenApiObject
--           & properties
--             L..~ fromList
--               [("descriptor", Inline st)]
--           & required L..~ ["descriptor"]

--       fulfillments =
--         toInlinedSchema (Proxy :: Proxy FulfillmentInfoV2)
--           & properties
--             L.<>~ fromList [("status", Inline descriptor)]
--           & required L.<>~ ["status"]
--   return $
--     NamedSchema (Just "NewMessageEventV2") $
--       mempty
--         & type_ L.?~ OpenApiObject
--         & properties
--           L..~ fromList
--             [ ("id", txt),
--               -- ("update_target", txt),
--               ("fulfillments", Inline fulfillments)
--             ]
--         & required L..~ ["id", "fulfillments"]

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data NewMessageEvent = NewMessageEvent
  { id :: Text,
    -- update_target :: Text,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON NewMessageEvent where
  toJSON NewMessageEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        -- <> "update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= ("descriptor" .= (("code" .= NEW_MESSAGE <> "name" .= A.String "New Message") :: A.Object) :: A.Object)))

instance FromJSON NewMessageEvent where
  parseJSON = withObject "NewMessageEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "descriptor") >>= (.: "code")
    unless (update_type == NEW_MESSAGE) $ fail "Wrong update_type."
    NewMessageEvent
      <$> obj .: "id"
      -- <*> obj .: "update_target"
      <*> obj .: "fulfillment"

instance ToSchema NewMessageEvent where
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
      NamedSchema (Just "NewMessageEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                -- ("update_target", txt),
                ("fulfillment", Inline fulfillment)
              ]
          & required L..~ ["id", "fulfillment"]
