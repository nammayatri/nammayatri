 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_COMPLETED))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, title, value)
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema

data RideCompletedEvent = RideCompletedEvent
  { id :: Text,
    update_target :: Text,
    quote :: RideCompletedQuote,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON RideCompletedEvent where
  toJSON RideCompletedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "quote" .= quote
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_COMPLETED) :: A.Object)))

instance FromJSON RideCompletedEvent where
  parseJSON = withObject "RideCompletedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_COMPLETED) $ fail "Wrong update_type."
    RideCompletedEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "quote"
      <*> obj .: "fulfillment"

instance ToSchema RideCompletedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    quote <- declareSchemaRef (Proxy :: Proxy RideCompletedQuote)
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
      NamedSchema (Just "RideCompletedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("quote", quote),
                ("fulfillment", Inline fulfillment)
              ]
          & required
            L..~ [ "id",
                   "./komn/update_target",
                   "quote",
                   "fulfillment"
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

data FulfillmentInfo = FulfillmentInfo
  { id :: Text, -- bppRideId
    chargeable_distance :: DecimalValue
  }
  deriving (Generic, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions fulfillmentInfoJSONOptions

instance FromJSON FulfillmentInfo where
  parseJSON = genericParseJSON fulfillmentInfoJSONOptions

instance ToJSON FulfillmentInfo where
  toJSON = genericToJSON fulfillmentInfoJSONOptions

fulfillmentInfoJSONOptions :: A.Options
fulfillmentInfoJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "chargeable_distance" -> "./komn/chargeable_distance"
        a -> a
    }
