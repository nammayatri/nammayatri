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
    PaymentChargeOption (..),
    paymentChargeOptions,
  )
where

import BecknV2.OnDemand.PaymentCharge (PaymentChargeDetails (..), PaymentChargeQuote (..), PlatformDeductions (..), customerPaymentChargeGross, paymentChargeDetailsTagList, paymentChargeGross, platformDeductionsAtPayment, platformDeductionsTagList, quotePaymentCharge)
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPMExtra
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer)
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

data PaymentChargeOption = PaymentChargeOption
  { paymentInstrument :: DMPMExtra.PaymentInstrument,
    paymentCharge :: HighPrecMoney,
    paymentChargeTax :: HighPrecMoney,
    paymentChargeBearer :: PaymentChargeBearer
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

paymentChargeOptions ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Currency ->
  HighPrecMoney ->
  m [PaymentChargeOption]
paymentChargeOptions merchantOperatingCityId currency payable = do
  paymentMethods <- getConfigList (MerchantPaymentMethodDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, configId = Nothing})
  pure
    [ PaymentChargeOption {paymentInstrument = paymentMethod.paymentInstrument, paymentCharge = paymentChargeGross quote, paymentChargeTax = quote.tax, paymentChargeBearer = quote.bearer}
      | paymentMethod <- paymentMethods,
        Just quote <- [paymentChargeDetailsOf paymentMethod >>= \config -> quotePaymentCharge currency config payable]
    ]
