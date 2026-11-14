module SharedLogic.Settlement
  ( netTaxExclusive,
    netTaxInclusive,
    netTotalTaxInclusive,
    unnetTaxExclusive,
    riderTaxRates,
    driverTaxRates,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.Settlement (PaymentChargeDetails (..))
import Control.Applicative ((<|>))
import Domain.Types.FarePolicy (FareChargeComponent (..), FareChargeConfig (..))
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..), PaymentChargeType (..))
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
import SharedLogic.FareCalculator (ParsedCodeValue (..), commissionExcludedComponents, parseCodeValue)

netTaxExclusive :: Maybe FareChargeConfig -> Maybe PaymentChargeDetails -> Tax.Regime -> Maybe FareChargeComponent -> Maybe Tax.TaxConfig -> HighPrecMoney -> HighPrecMoney
netTaxExclusive mbCommissionConfig mbPaymentChargeDetails taxRegime mbComponent mbTaxConfig amount =
  gross - govtTax - amount * commissionRate - gross * paymentChargeRate
  where
    gross = amount * (1 + Tax.taxRateOf mbTaxConfig)
    govtTax = if taxRegime == Tax.GST then gross - amount else 0
    commissionRate = case (mbCommissionConfig, mbComponent) of
      (Just config, Just component)
        | component `notElem` commissionExcludedComponents,
          component `elem` (if null config.appliesOn then [RideFare] else config.appliesOn),
          Just (ParsedPercentage pct) <- parseCodeValue config.value ->
          HighPrecMoney pct
      _ -> 0
    paymentChargeRate = driverPaymentChargeRate mbPaymentChargeDetails

netTaxInclusive :: Maybe PaymentChargeDetails -> Tax.Regime -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney -> HighPrecMoney
netTaxInclusive mbPaymentChargeDetails taxRegime mbTax mbCancellationCommission amount =
  amount - (if taxRegime == Tax.GST then fromMaybe 0 mbTax else 0) - fromMaybe 0 mbCancellationCommission - amount * driverPaymentChargeRate mbPaymentChargeDetails

driverPaymentChargeRate :: Maybe PaymentChargeDetails -> HighPrecMoney
driverPaymentChargeRate = \case
  Just details | details.bearer == PAYMENT_DRIVER, details.chargeType == PERCENTAGE -> details.chargeValue / 100 * (1 + Tax.taxRateOf details.chargeTax)
  _ -> 0

netTotalTaxInclusive :: Tax.Regime -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe PaymentChargeBearer -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney
netTotalTaxInclusive taxRegime mbCommission mbPaymentCharge mbBearer componentTax airportEntryFeeWithTax total =
  max 0 (total - fromMaybe 0 mbCommission - driverCharge - (if taxRegime == Tax.GST then componentTax else 0) - airportEntryFeeWithTax)
  where
    driverCharge = if mbBearer == Just PAYMENT_DRIVER then fromMaybe 0 mbPaymentCharge else 0

unnetTaxExclusive :: Maybe FareChargeConfig -> Maybe PaymentChargeDetails -> Tax.Regime -> Maybe FareChargeComponent -> Maybe Tax.TaxConfig -> HighPrecMoney -> HighPrecMoney
unnetTaxExclusive mbCommissionConfig mbPaymentChargeDetails taxRegime mbComponent mbTaxConfig amount =
  if netOfOne > 0 then amount / netOfOne else amount
  where
    netOfOne = netTaxExclusive mbCommissionConfig mbPaymentChargeDetails taxRegime mbComponent mbTaxConfig 1

driverTaxRates :: DTC.TransporterConfig -> Maybe Tax.TaxConfig -> Maybe Tax.TaxConfig -> Maybe Tax.TaxConfig -> Maybe Tax.TaxConfig -> Maybe PaymentChargeDetails -> Tax.DriverTaxRates
driverTaxRates transporterConfig mbRideTax mbTollTax mbParkingTax mbCommissionTax mbPaymentChargeDetails =
  Tax.DriverTaxRates
    { regime = riderRates.regime,
      riderPayable = riderRates.riderPayable,
      driverDeductible = Tax.DriverDeductibleTaxRates {commission = mbCommissionTax, paymentCharge = chargeTaxBorneBy PAYMENT_DRIVER}
    }
  where
    riderRates = riderTaxRates transporterConfig mbRideTax mbTollTax mbParkingTax (chargeTaxBorneBy PAYMENT_CUSTOMER)
    chargeTaxBorneBy bearer = mbPaymentChargeDetails >>= \details -> if details.bearer == bearer then details.chargeTax else Nothing

riderTaxRates :: DTC.TransporterConfig -> Maybe Tax.TaxConfig -> Maybe Tax.TaxConfig -> Maybe Tax.TaxConfig -> Maybe Tax.TaxConfig -> Tax.RiderTaxRates
riderTaxRates transporterConfig mbRideTax mbTollTax mbParkingTax mbPaymentChargeTax =
  Tax.RiderTaxRates
    { regime = taxRegime,
      riderPayable =
        FB.RideTaxRates
          { rideFare = mbRideTax <|> gstTaxConfig taxConfig.rideGst,
            toll = mbTollTax,
            parking = mbParkingTax,
            airportEntryFee = taxConfig.airportEntryFeeTaxConfig <|> gstTaxConfig (fromMaybe taxConfig.rideGst taxConfig.airportEntryFeeGst),
            cancellationFee = Nothing,
            platformFee = Nothing,
            paymentCharge = mbPaymentChargeTax
          }
    }
  where
    taxConfig = transporterConfig.taxConfig
    taxRegime = fromMaybe Tax.GST taxConfig.taxRegime
    gstTaxConfig gst =
      let asPct = fmap (\fraction -> realToFrac (fraction * 100))
          totalPct = sum (catMaybes [asPct gst.cgstPercentage, asPct gst.sgstPercentage, asPct gst.igstPercentage])
       in if taxRegime == Tax.GST && totalPct > 0
            then Just Tax.TaxConfig {tax = totalPct, cgst = asPct gst.cgstPercentage, sgst = asPct gst.sgstPercentage, igst = asPct gst.igstPercentage}
            else Nothing
