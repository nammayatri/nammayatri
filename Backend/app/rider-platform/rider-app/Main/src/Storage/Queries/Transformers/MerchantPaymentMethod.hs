module Storage.Queries.Transformers.MerchantPaymentMethod where

import Domain.Types.Extra.MerchantPaymentMethod

paymentTypeTrans :: (Domain.Types.Extra.MerchantPaymentMethod.PaymentType -> Domain.Types.Extra.MerchantPaymentMethod.PaymentType)
paymentTypeTrans paymentType = case paymentType of
  ON_FULFILLMENT -> ON_FULFILLMENT
  POSTPAID -> ON_FULFILLMENT
