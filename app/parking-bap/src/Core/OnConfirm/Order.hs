module Core.OnConfirm.Order where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Billing
import Core.Common.Payment
import Core.Common.Price
import Core.OnConfirm.Fulfillment
import Core.OnConfirm.Item
import Core.OnConfirm.Provider
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Order = Order
  { id :: Text,
    state :: Maybe OrderState,
    provider :: Provider,
    provider_location :: OrderProviderLocation,
    items :: [Item],
    billing :: Billing,
    fulfillment :: Fulfillment,
    quote :: SpecQuote,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data OrderState = ACTIVE | CANCELLED | COMPLETE
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderState where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype OrderProviderLocation = OrderProviderLocation
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OrderProviderLocation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data SpecQuote = SpecQuote
  { price :: Price,
    breakup :: [Breakup]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data Breakup = Breakup
  { title :: Text,
    price :: Price
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
