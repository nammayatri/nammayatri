{-# LANGUAGE ApplicativeDo #-}

module SharedLogic.MerchantPaymentMethod where

import qualified Domain.Types.Booking as DRB
import Domain.Types.MerchantPaymentMethod
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM

mkPaymentMethodInfo :: MerchantPaymentMethod -> PaymentMethodInfo
mkPaymentMethodInfo MerchantPaymentMethod {..} = PaymentMethodInfo {..}

-- | Resolves the payment method info of a booking. A missing merchant payment method
-- is logged and treated as Nothing instead of failing the caller.
resolveBookingPaymentMethodInfo :: (CacheFlow m r, EsqDBFlow m r) => DRB.Booking -> m (Maybe PaymentMethodInfo)
resolveBookingPaymentMethodInfo booking =
  case booking.paymentMethodId of
    Nothing -> pure Nothing
    Just paymentMethodId ->
      CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId >>= \case
        Just merchantPaymentMethod -> pure . Just $ mkPaymentMethodInfo merchantPaymentMethod
        Nothing -> do
          logError $ "MerchantPaymentMethod not found for bookingId: " <> booking.id.getId <> ", paymentMethodId: " <> paymentMethodId.getId
          pure Nothing

getPostpaidPaymentUrl :: MerchantPaymentMethod -> Maybe Text
getPostpaidPaymentUrl mpm = do
  if mpm.paymentType == ON_FULFILLMENT && mpm.collectedBy == BPP && mpm.paymentInstrument `notElem` [Cash, BoothOnline]
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
