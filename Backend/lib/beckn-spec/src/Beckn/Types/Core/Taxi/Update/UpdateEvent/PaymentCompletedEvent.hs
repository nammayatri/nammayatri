{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent
  ( module Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent.Payment as Reexport
import Beckn.Types.Core.Taxi.Update.UpdateEvent.UpdateEventType (UpdateEventType (PAYMENT_COMPLETED))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, title, value)
import EulerHS.Prelude hiding (fromList, id)
import GHC.Exts (fromList)
import Kernel.Utils.JSON
import Kernel.Utils.Schema

data PaymentCompletedEventV2 = PaymentCompletedEventV2
  { id :: Text,
    -- update_target :: Text, -- Moved to UpdateMessageV2
    fulfillments :: [FulfillmentInfo], -- No need to change this because it only has id -> Just converted to list
    payments :: [PaymentV2]
  }
  deriving (Generic, Show)

instance ToJSON PaymentCompletedEventV2 where
  toJSON = genericToJSON removeNullFields

instance FromJSON PaymentCompletedEventV2 where
  parseJSON = genericParseJSON removeNullFields

instance ToSchema PaymentCompletedEventV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype FulfillmentInfo = FulfillmentInfo
  { id :: Text -- bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data PaymentCompletedEvent = PaymentCompletedEvent
  { id :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo,
    payment :: Payment
  }
  deriving (Generic, Show)

instance ToJSON PaymentCompletedEvent where
  toJSON PaymentCompletedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "payment" .= payment
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= PAYMENT_COMPLETED) :: A.Object)))

instance FromJSON PaymentCompletedEvent where
  parseJSON = withObject "PaymentCompletedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == PAYMENT_COMPLETED) $ fail "Wrong update_type."
    PaymentCompletedEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "fulfillment"
      <*> obj .: "payment"

instance ToSchema PaymentCompletedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    payment <- declareSchemaRef (Proxy :: Proxy Payment)
    update_type <- declareSchemaRef (Proxy :: Proxy UpdateEventType)
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
      NamedSchema (Just "PaymentCompletedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("payment", payment),
                ("fulfillment", Inline fulfillment)
              ]
          & required
            L..~ [ "id",
                   "./komn/update_target",
                   "fulfillment",
                   "payment"
                 ]
