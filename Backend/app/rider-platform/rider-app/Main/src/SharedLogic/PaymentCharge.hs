module SharedLogic.PaymentCharge
  ( PaymentChargeDetails (..),
    PaymentChargeQuote (..),
    quotePaymentCharge,
    paymentChargeDetailsOf,
    resolvePaymentChargeDetails,
    resolvePaymentCharge,
    paymentChargeGross,
    customerPaymentChargeGross,
    paymentChargeDetailsTagList,
    PlatformDeductions (..),
    platformDeductionsAtPayment,
    platformDeductionsTagList,
    merchantPaymentMethods,
    PayableAmount (..),
    PayableOption (..),
    PaymentChargeBase (..),
    mkPayableAmount,
    mkFarePayable,
    mkChargePayable,
    chargeAmountFromPayable,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.PaymentCharge (PaymentChargeDetails (..), PaymentChargeQuote (..), PlatformDeductions (..), customerPaymentChargeGross, paymentChargeDetailsTagList, paymentChargeGross, paymentChargeRateBorneBy, platformDeductionsAtPayment, platformDeductionsTagList, quotePaymentCharge, quotePaymentChargeShare)
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPMExtra
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Lib.ConfigPilot.Interface.Types (getConfigList)
import Storage.ConfigPilot.Config.MerchantPaymentMethod (MerchantPaymentMethodDimensions (..))

paymentChargeDetailsOf :: DMPM.MerchantPaymentMethod -> Maybe PaymentChargeDetails
paymentChargeDetailsOf paymentMethod = do
  guard (paymentMethod.paymentInstrument /= DMPMExtra.Cash)
  chargeType <- paymentMethod.paymentChargeType
  chargeValue <- paymentMethod.paymentCharge
  bearer <- paymentMethod.paymentChargeBearer
  pure PaymentChargeDetails {chargeType, chargeValue, chargeTax = paymentMethod.paymentChargeTax, bearer}

resolvePaymentChargeDetails ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPMExtra.PaymentInstrument ->
  m (Maybe PaymentChargeDetails)
resolvePaymentChargeDetails merchantOperatingCityId mbInstrument = case mbInstrument of
  Nothing -> pure Nothing
  Just instrument -> do
    paymentMethods <- getConfigList (MerchantPaymentMethodDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, configId = Nothing})
    pure $ find ((== instrument) . (.paymentInstrument)) paymentMethods >>= paymentChargeDetailsOf

resolvePaymentCharge ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPMExtra.PaymentInstrument ->
  Currency ->
  HighPrecMoney ->
  m (Maybe PaymentChargeQuote)
resolvePaymentCharge merchantOperatingCityId mbInstrument currency payable =
  (>>= \config -> quotePaymentCharge currency config payable) <$> resolvePaymentChargeDetails merchantOperatingCityId mbInstrument

merchantPaymentMethods ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DMPM.MerchantPaymentMethod]
merchantPaymentMethods merchantOperatingCityId = getConfigList (MerchantPaymentMethodDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, configId = Nothing})

data PaymentChargeBase = WholeFare | ShareOfFare

data PayableAmount = PayableAmount
  { amount :: HighPrecMoney,
    currency :: Currency,
    paymentOptions :: [PayableOption]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PayableOption = PayableOption
  { paymentInstrument :: DMPMExtra.PaymentInstrument,
    paymentChargeBearer :: Maybe PaymentChargeBearer,
    amount :: HighPrecMoney,
    breakup :: Tax.PayableBreakup
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

mkPayableAmount :: Currency -> [DMPM.MerchantPaymentMethod] -> PaymentChargeBase -> HighPrecMoney -> HighPrecMoney -> PayableAmount
mkPayableAmount payableCurrency paymentMethods chargeBase taxExclusiveAmount taxAmount =
  PayableAmount
    { amount = taxExclusiveAmount,
      currency = payableCurrency,
      paymentOptions = map optionOf paymentMethods
    }
  where
    quoteOf = case chargeBase of
      WholeFare -> quotePaymentCharge
      ShareOfFare -> quotePaymentChargeShare
    optionOf paymentMethod =
      let mbQuote = paymentChargeDetailsOf paymentMethod >>= \config -> quoteOf payableCurrency config (taxExclusiveAmount + taxAmount)
          breakup = Tax.mkPayableBreakup payableCurrency taxAmount (customerPaymentChargeGross mbQuote)
       in PayableOption
            { paymentInstrument = paymentMethod.paymentInstrument,
              paymentChargeBearer = (.bearer) <$> mbQuote,
              amount = Tax.payableTotal payableCurrency taxExclusiveAmount breakup,
              breakup
            }

mkFarePayable :: Currency -> [DMPM.MerchantPaymentMethod] -> HighPrecMoney -> HighPrecMoney -> PayableAmount
mkFarePayable payableCurrency paymentMethods taxInclusiveFare fareTax = mkPayableAmount payableCurrency paymentMethods WholeFare (taxInclusiveFare - fareTax) fareTax

mkChargePayable :: Currency -> Maybe Tax.RiderTaxRates -> [DMPM.MerchantPaymentMethod] -> HighPrecMoney -> PayableAmount
mkChargePayable payableCurrency mbRiderTaxRates paymentMethods chargeAmount =
  mkPayableAmount payableCurrency paymentMethods ShareOfFare chargeAmount (Tax.riderTaxedExclusive payableCurrency FB.rideFare mbRiderTaxRates chargeAmount).tax

chargeAmountFromPayable :: Currency -> Maybe Tax.RiderTaxRates -> Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
chargeAmountFromPayable payableCurrency mbRiderTaxRates mbPaymentCharge =
  Tax.exclusiveFromPayable payableCurrency (maybe 0 (\rates -> Tax.taxRateOf rates.regime (FB.rideFare rates.riderPayable)) mbRiderTaxRates) (paymentChargeRateBorneBy PAYMENT_CUSTOMER mbPaymentCharge)
