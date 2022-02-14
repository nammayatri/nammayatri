module Core.Spec.OnStatus.Order where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Common.Billing
import Core.Spec.Common.OrderState
import Core.Spec.Common.Payment
import Core.Spec.Common.ProviderId
import Core.Spec.Common.Quotation
import Core.Spec.OnStatus.Item
import Core.Spec.OnStatus.Params
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Order = Order
  { state :: State,
    provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
