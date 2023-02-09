{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Spec.OnConfirm.Order where

import Beckn.Spec.Common.Billing
import Beckn.Spec.Common.OrderState
import Beckn.Spec.Common.Payment
import Beckn.Spec.Common.ProviderId
import Beckn.Spec.Common.Quotation
import Beckn.Spec.OnConfirm.Item
import Beckn.Spec.OnConfirm.Params
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
