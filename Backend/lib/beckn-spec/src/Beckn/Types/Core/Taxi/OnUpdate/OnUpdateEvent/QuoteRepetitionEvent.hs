{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.QuoteRepetitionEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.QuoteRepetitionEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.Common.FulfillmentInfo
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (QUOTE_REPETITION))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import GHC.Exts (fromList)
import Kernel.Prelude
import Kernel.Utils.Schema

data QuoteRepetitionEvent = QuoteRepetitionEvent
  { id :: Text, -- bppBookingId
  -- update_target :: Text,
    fulfillment :: FulfillmentInfo,
    item :: Item
  }
  deriving (Generic, Show)

instance ToJSON QuoteRepetitionEvent where
  toJSON QuoteRepetitionEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        -- <> "update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= ("descriptor" .= (("code" .= QUOTE_REPETITION <> "name" .= A.String "Quote Repetition") :: A.Object) :: A.Object)))
        <> "item" .= item

instance FromJSON QuoteRepetitionEvent where
  parseJSON = withObject "QuoteRepetitionEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "descriptor") >>= (.: "code")
    unless (update_type == QUOTE_REPETITION) $ fail "Wrong update_type."
    QuoteRepetitionEvent
      <$> obj .: "id"
      -- <*> obj .: "update_target"
      <*> obj .: "fulfillment"
      <*> obj .: "item"

instance ToSchema QuoteRepetitionEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    reallocationSource <- declareSchemaRef (Proxy :: Proxy CancellationSource)
    item <- declareSchemaRef (Proxy :: Proxy Item)
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
      NamedSchema (Just "QuoteRepetitionEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                -- ("update_target", txt),
                ("cancellation_reason", reallocationSource),
                ("fulfillment", Inline fulfillment),
                ("item", item)
              ]
          & required L..~ ["id", "cancellation_reason", "fulfillment", "item"]

newtype Item = Item
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
