{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.MerchantPaymentMethod where

import Domain.Types.MerchantPaymentMethod
import Kernel.Prelude
import Kernel.Types.Id

data PaymentMethodAPIEntity = PaymentMethodAPIEntity
  { id :: Id MerchantPaymentMethod,
    paymentType :: PaymentType,
    paymentInstrument :: PaymentInstrument,
    collectedBy :: PaymentCollector
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
