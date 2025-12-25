{-# LANGUAGE ApplicativeDo #-}

module SharedLogic.MerchantPaymentMethod where

import Domain.Types.MerchantPaymentMethod
import Kernel.Prelude

mkPaymentMethodInfo :: MerchantPaymentMethod -> PaymentMethodInfo
mkPaymentMethodInfo MerchantPaymentMethod {..} = PaymentMethodInfo {..}

getPostpaidPaymentUrl :: MerchantPaymentMethod -> Maybe Text
getPostpaidPaymentUrl mpm = do
  if mpm.paymentType == ON_FULFILLMENT && mpm.collectedBy == BPP && mpm.paymentInstrument /= Cash
    then Just $ mkDummyPaymentUrl mpm
    else Nothing

mkDummyPaymentUrl :: MerchantPaymentMethod -> Text
mkDummyPaymentUrl MerchantPaymentMethod {..} = do
  "payment_link_for_paymentInstrument="
    <> show paymentInstrument
    <> ";collectedBy="
    <> show collectedBy
    <> ";paymentType="
    <> show paymentType
