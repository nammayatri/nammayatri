module Core.Order where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnConfirm.Billing
import Core.OnConfirm.Fulfillment
import Core.OnConfirm.Item
import Core.OnConfirm.Payment
import Core.OnConfirm.Provider
import Core.OnConfirm.SpecQuote
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data OrderState = ACTIVE | CANCELLED | COMPLETE
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema OrderState where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype OrderProviderLocation = OrderProviderLocation
  { id :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OrderProviderLocation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

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
  deriving (Generic, FromJSON, ToJSON, ToSchema)
