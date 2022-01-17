module Core.Confirm.Order where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Confirm.Billing
import Core.Confirm.Fulfillment
import Core.Confirm.Item
import Core.Confirm.Provider
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Order = Order
  { provider :: Provider,
    items :: [Item],
    billing :: Billing,
    fulfillment :: Fulfillment
  }
  deriving (Generic, ToJSON)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
