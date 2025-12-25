module Storage.Queries.Transformers.MerchantPaymentMethod where

import qualified Domain.Types.MerchantPaymentMethod

getPaymentType :: (Domain.Types.MerchantPaymentMethod.PaymentType -> Domain.Types.MerchantPaymentMethod.PaymentType)
getPaymentType _paymentType = case _paymentType of
  Domain.Types.MerchantPaymentMethod.ON_FULFILLMENT -> Domain.Types.MerchantPaymentMethod.ON_FULFILLMENT
  Domain.Types.MerchantPaymentMethod.POSTPAID -> Domain.Types.MerchantPaymentMethod.ON_FULFILLMENT
