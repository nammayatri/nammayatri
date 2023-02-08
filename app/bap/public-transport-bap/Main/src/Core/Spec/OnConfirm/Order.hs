{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Spec.OnConfirm.Order where

import Core.Spec.Common.Billing
import Core.Spec.Common.OrderState
import Core.Spec.Common.Payment
import Core.Spec.Common.ProviderId
import Core.Spec.Common.Quotation
import Core.Spec.OnConfirm.Item
import Core.Spec.OnConfirm.Params
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema

data Order = Order
  { id :: Text,
    state :: State,
    provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

deriving instance PrettyShow (Payment Params)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
