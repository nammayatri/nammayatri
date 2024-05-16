{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.MerchantPaymentMethod where

import Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Extra.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)

paymentTypeTrans :: (Domain.Types.Extra.MerchantPaymentMethod.PaymentType -> Domain.Types.Extra.MerchantPaymentMethod.PaymentType)
paymentTypeTrans paymentType = case paymentType of
  ON_FULFILLMENT -> ON_FULFILLMENT
  POSTPAID -> ON_FULFILLMENT
