{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Spec.OnConfirm.Order where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)
import Core.Spec.Common.Billing
import Core.Spec.Common.OrderState
import Core.Spec.Common.Payment
import Core.Spec.Common.ProviderId
import Core.Spec.Common.Quotation
import Core.Spec.OnConfirm.Item
import Core.Spec.OnConfirm.Params

data Order = Order
  { id :: Text,
    state :: State,
    provider :: ProviderId,
    items :: [Item],
    billing :: Billing,
    quote :: Quotation,
    payment :: Payment Params
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)

deriving instance ToSchema (Payment Params)

deriving instance PrettyShow (Payment Params)
