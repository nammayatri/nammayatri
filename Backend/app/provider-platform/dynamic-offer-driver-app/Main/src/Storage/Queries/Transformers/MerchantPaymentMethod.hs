{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.MerchantPaymentMethod where

import qualified Domain.Types.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

getPaymentType :: (Domain.Types.MerchantPaymentMethod.PaymentType -> Domain.Types.MerchantPaymentMethod.PaymentType)
getPaymentType _paymentType = case _paymentType of
  Domain.Types.MerchantPaymentMethod.ON_FULFILLMENT -> Domain.Types.MerchantPaymentMethod.ON_FULFILLMENT
  Domain.Types.MerchantPaymentMethod.POSTPAID -> Domain.Types.MerchantPaymentMethod.ON_FULFILLMENT
