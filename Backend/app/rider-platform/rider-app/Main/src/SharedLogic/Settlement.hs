module SharedLogic.Settlement
  ( resolvePaymentChargeDetails,
    bookingPaymentChargeDetails,
    grossTaxExclusive,
    grossTaxInclusive,
    grossTotalTaxInclusive,
    ungrossTaxExclusive,
    ungrossTaxInclusive,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.Settlement (GrossMultiplier (..), PaymentChargeDetails (..), grossMultipliers, riderGross)
import qualified Domain.Types.Booking as DRB
import Domain.Types.Extra.MerchantPaymentMethod (PaymentInstrument)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow)
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM

resolvePaymentChargeDetails :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe PaymentInstrument -> m (Maybe PaymentChargeDetails)
resolvePaymentChargeDetails merchantOperatingCityId = \case
  Nothing -> pure Nothing
  Just instrument -> do
    paymentMethods <- CQMPM.findAllByMerchantOperatingCityId merchantOperatingCityId
    pure $ do
      paymentMethod <- find ((== instrument) . (.paymentInstrument)) (paymentMethods :: [DMPM.MerchantPaymentMethod])
      chargeType <- paymentMethod.paymentChargeType
      chargeValue <- paymentMethod.paymentCharge
      bearer <- paymentMethod.paymentChargeBearer
      pure PaymentChargeDetails {chargeType, chargeValue, chargeTax = paymentMethod.paymentChargeTax, bearer}

bookingPaymentChargeDetails :: (CacheFlow m r, EsqDBFlow m r) => DRB.Booking -> m (Maybe PaymentChargeDetails)
bookingPaymentChargeDetails booking
  | isJust booking.paymentChargeBearer = resolvePaymentChargeDetails booking.merchantOperatingCityId booking.paymentInstrument
  | otherwise = pure Nothing

multiplierOf :: (FB.RideTaxRates GrossMultiplier -> GrossMultiplier) -> Maybe PaymentChargeDetails -> Maybe Tax.RiderTaxRates -> GrossMultiplier
multiplierOf component mbPaymentCharge mbRates =
  component (grossMultipliers mbPaymentCharge (fromMaybe Tax.RiderTaxRates {regime = Tax.VAT, riderPayable = pure Nothing} mbRates))

grossTaxExclusive :: (FB.RideTaxRates GrossMultiplier -> GrossMultiplier) -> Maybe PaymentChargeDetails -> Maybe Tax.RiderTaxRates -> HighPrecMoney -> HighPrecMoney
grossTaxExclusive component mbPaymentCharge mbRates amount = amount * (multiplierOf component mbPaymentCharge mbRates).taxExclusive

grossTaxInclusive :: Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
grossTaxInclusive mbPaymentCharge amount = amount * (multiplierOf (.rideFare) mbPaymentCharge Nothing).taxInclusive

grossTotalTaxInclusive :: Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
grossTotalTaxInclusive = riderGross

ungrossTaxExclusive :: (FB.RideTaxRates GrossMultiplier -> GrossMultiplier) -> Maybe PaymentChargeDetails -> Maybe Tax.RiderTaxRates -> HighPrecMoney -> HighPrecMoney
ungrossTaxExclusive component mbPaymentCharge mbRates amount = amount / (multiplierOf component mbPaymentCharge mbRates).taxExclusive

ungrossTaxInclusive :: Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
ungrossTaxInclusive mbPaymentCharge amount = amount / (multiplierOf (.rideFare) mbPaymentCharge Nothing).taxInclusive
