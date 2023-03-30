{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.FulfillmentInfo
import Beckn.Types.Core.Taxi.Common.Payment as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_COMPLETED))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, tags, title, value)
import EulerHS.Prelude hiding (id)
import GHC.Exts (fromList)
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema

data RideCompletedEvent = RideCompletedEvent
  { id :: Text,
    -- update_target :: Text,
    quote :: RideCompletedQuote,
    fulfillment :: FulfillmentInfo,
    payment :: Maybe Payment
  }
  deriving (Generic, Show)

instance ToJSON RideCompletedEvent where
  toJSON RideCompletedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        -- <> "update_target" .= update_target
        <> "quote" .= quote
        <> "payment" .= payment
        <> "fulfillment" .= (fulfJSON <> ("state" .= ("descriptor" .= (("code" .= RIDE_COMPLETED <> "name" .= A.String "Ride Completed") :: A.Object) :: A.Object)))

instance FromJSON RideCompletedEvent where
  parseJSON = withObject "RideCompletedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "descriptor") >>= (.: "code")
    unless (update_type == RIDE_COMPLETED) $ fail "Wrong update_type."
    RideCompletedEvent
      <$> obj .: "id"
      -- <*> obj .: "update_target"
      <*> obj .: "quote"
      <*> obj .: "fulfillment"
      <*> obj .: "payment"

instance ToSchema RideCompletedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    quote <- declareSchemaRef (Proxy :: Proxy RideCompletedQuote)
    payment <- declareSchemaRef (Proxy :: Proxy Payment)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    let st =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("code", update_type)]
            & required L..~ ["code"]
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
      NamedSchema (Just "RideCompletedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                -- ("update_target", txt),
                ("quote", quote),
                ("payment", payment),
                ("fulfillment", Inline fulfillment)
              ]
          & required
            L..~ [ "id",
                   --  "update_target",
                   "quote",
                   "fulfillment",
                   "payment"
                 ]

data RideCompletedQuote = RideCompletedQuote
  { price :: QuotePrice,
    breakup :: [BreakupItem]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema RideCompletedQuote where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data QuotePrice = QuotePrice
  { currency :: Text,
    value :: DecimalValue,
    computed_value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema QuotePrice where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BreakupItem = BreakupItem
  { title :: Text,
    price :: BreakupPrice
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow)

instance ToSchema BreakupItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BreakupPrice = BreakupPrice
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow)

instance ToSchema BreakupPrice where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data DistanceRelatedTags = DistanceRelatedTags
  { chargeable_distance :: DecimalValue,
    traveled_distance :: DecimalValue
  }
