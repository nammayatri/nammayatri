module SharedLogic.ComponentCharge
  ( FareChargeRates (..),
    ComponentChargeConfig (..),
    IncludedCharges (..),
    componentChargeConfig,
    mkAmountBreakup,
    mkNetAmount,
    mkGrossAmount,
    mkInclusiveAmountBreakup,
    mkInclusiveNetAmount,
    wholeFarePaymentCharge,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.PaymentCharge (PaymentChargeDetails (..), paymentChargeGross, paymentChargeRateBorneBy, paymentChargeShareBorneBy, quotePaymentCharge)
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import Kernel.Prelude
import Kernel.Types.Common
import qualified SharedLogic.FareCalculator as FC

data FareChargeRates = FareChargeRates
  { currency :: Currency,
    commissionChargeConfig :: Maybe DFP.FareChargeConfig,
    rideTax :: Maybe Tax.RideTax,
    paymentChargeDetails :: Maybe PaymentChargeDetails
  }

data ComponentChargeConfig = ComponentChargeConfig
  { currency :: Currency,
    taxRegime :: Maybe Tax.Regime,
    taxConfig :: Maybe Tax.TaxConfig,
    commissionChargeConfig :: Maybe DFP.FareChargeConfig,
    component :: Maybe DFP.FareChargeComponent,
    paymentChargeDetails :: Maybe PaymentChargeDetails
  }

data IncludedCharges = IncludedCharges
  { tax :: HighPrecMoney,
    commission :: HighPrecMoney,
    paymentCharge :: Maybe HighPrecMoney
  }

componentChargeConfig :: FareChargeRates -> Maybe DFP.FareChargeComponent -> ComponentChargeConfig
componentChargeConfig rates mbComponent =
  ComponentChargeConfig
    { currency = rates.currency,
      taxRegime = (.regime) <$> rates.rideTax,
      taxConfig = rates.rideTax >>= componentTaxConfig . (.riderPayable),
      commissionChargeConfig = rates.commissionChargeConfig,
      component = mbComponent,
      paymentChargeDetails = rates.paymentChargeDetails
    }
  where
    componentTaxConfig = case mbComponent of
      Just DFP.TollChargesComponent -> (.toll)
      Just DFP.ParkingChargeComponent -> (.parking)
      _ -> (.rideFare)

taxOn :: ComponentChargeConfig -> HighPrecMoney -> HighPrecMoney
taxOn config amount = maybe 0 (\regime -> (Tax.taxedExclusive config.currency regime config.taxConfig amount).tax) config.taxRegime

commissionOn :: ComponentChargeConfig -> HighPrecMoney -> HighPrecMoney
commissionOn config amount = maybe 0 (\fareComponent -> FC.commissionOnComponent config.commissionChargeConfig fareComponent amount) config.component

paymentChargeOn :: ComponentChargeConfig -> HighPrecMoney -> HighPrecMoney
paymentChargeOn config = paymentChargeShareBorneBy PAYMENT_DRIVER config.currency config.paymentChargeDetails

deductionRates :: ComponentChargeConfig -> Tax.DeductionRates
deductionRates config =
  Tax.DeductionRates
    { taxRate = maybe 0 (\regime -> Tax.taxRateOf regime config.taxConfig) config.taxRegime,
      commissionRate = commissionOn config 1,
      paymentChargeRate = paymentChargeRateBorneBy PAYMENT_DRIVER config.paymentChargeDetails
    }

mkAmountBreakup :: ComponentChargeConfig -> HighPrecMoney -> Tax.AmountBreakup
mkAmountBreakup config amount =
  Tax.mkAmountBreakup config.currency taxAmount (commissionOn config amount) (paymentChargeOn config (amount + taxAmount))
  where
    taxAmount = taxOn config amount

mkNetAmount :: ComponentChargeConfig -> HighPrecMoney -> (HighPrecMoney, Tax.AmountBreakup)
mkNetAmount config amount =
  let breakup = mkAmountBreakup config amount
   in (Tax.netOfExclusive config.currency amount breakup, breakup)

mkGrossAmount :: ComponentChargeConfig -> [HighPrecMoney] -> HighPrecMoney -> HighPrecMoney
mkGrossAmount config knownGrossAmounts net =
  fromMaybe (Tax.grossFromNet config.currency (deductionRates config) net) $
    find (\gross -> fst (mkNetAmount config gross) == net) knownGrossAmounts

mkInclusiveAmountBreakup :: ComponentChargeConfig -> IncludedCharges -> HighPrecMoney -> Tax.AmountBreakup
mkInclusiveAmountBreakup config included amount =
  Tax.mkAmountBreakup config.currency included.tax included.commission (fromMaybe (paymentChargeOn config amount) included.paymentCharge)

mkInclusiveNetAmount :: ComponentChargeConfig -> IncludedCharges -> HighPrecMoney -> (HighPrecMoney, Tax.AmountBreakup)
mkInclusiveNetAmount config included amount =
  let breakup = mkInclusiveAmountBreakup config included amount
   in (Tax.netOfInclusive config.currency amount breakup, breakup)

wholeFarePaymentCharge :: ComponentChargeConfig -> HighPrecMoney -> HighPrecMoney
wholeFarePaymentCharge config amount =
  maybe 0 (\details -> if details.bearer == PAYMENT_DRIVER then maybe 0 paymentChargeGross (quotePaymentCharge config.currency details amount) else 0) config.paymentChargeDetails
