module SharedLogic.RideTax
  ( resolveTaxRates,
    resolveCancellationTax,
    resolveTaxBreakups,
    TaxableAmount (..),
    TaxableAmounts (..),
    noTaxableAmounts,
    fareTaxesOf,
    fareTaxTotal,
    cityRideTax,
    bookingTax,
    bookingRideTax,
    taxPctOf,
    gstBreakupOf,
    splitInclusive,
    commissionTaxPct,
    splitCommissionInclusive,
    gstBreakupFromTax,
    addTaxBreakup,
    scaleTaxBreakup,
  )
where

import BecknV2.OnDemand.FareBreakup.Core (RideTaxRates (..))
import BecknV2.OnDemand.FareBreakup.Tax (TaxBreakup (..))
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified BecknV2.OnDemand.PaymentCharge as PaymentCharge
import Control.Applicative ((<|>))
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.CancellationConsequenceMatrix as DCCM
import qualified Domain.Types.Extra.CancellationConsequenceMatrix as DExtra
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney (..), roundAmountByCurrency')

resolveTaxRates ::
  ( HasField "rideTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "tollTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "parkingTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "commissionTaxConfig" fp (Maybe Tax.TaxConfig)
  ) =>
  DTC.TransporterConfig ->
  Maybe fp ->
  Maybe PaymentCharge.PaymentChargeDetails ->
  Tax.RideTax
resolveTaxRates tc mbFarePolicy mbPaymentCharge =
  Tax.RideTax
    { regime = taxRegime,
      riderPayable =
        RideTaxRates
          { rideFare = cityRate rideTax,
            toll = cityRate (mbFarePolicy >>= (.tollTaxConfig)),
            parking = cityRate (mbFarePolicy >>= (.parkingTaxConfig)),
            airportEntryFee = cityRate (tc.taxConfig.airportEntryFeeTaxConfig <|> gstOnly (gstTax (fromMaybe tc.taxConfig.rideGst tc.taxConfig.airportEntryFeeGst))),
            cancellationFee = Nothing,
            paymentCharge = paymentChargeRateBorneBy [PAYMENT_CUSTOMER]
          },
      driverDeductible =
        Tax.DriverDeductible
          { commission = mbFarePolicy >>= (.commissionTaxConfig),
            cancellationCommission = Nothing,
            paymentCharge = paymentChargeRateBorneBy [PAYMENT_CUSTOMER, PAYMENT_DRIVER]
          },
      platformDeductible = Tax.PlatformDeductible {paymentCharge = paymentChargeRateBorneBy [PAYMENT_PLATFORM]}
    }
  where
    taxRegime = regimeOf tc
    cityRate t = if taxRegime == Tax.NONE then Nothing else t
    gstOnly t = if taxRegime == Tax.GST then t else Nothing
    rideTax = (mbFarePolicy >>= (.rideTaxConfig)) <|> gstOnly (gstTax tc.taxConfig.rideGst)
    paymentChargeRateBorneBy bearers = mfilter ((`elem` bearers) . (.bearer)) mbPaymentCharge >>= (.chargeTax)

resolveCancellationTax :: DCCM.CancellationConsequenceMatrix -> Tax.RideTax -> Tax.RideTax
resolveCancellationTax row rideTax =
  Tax.RideTax
    { regime = rideTax.regime,
      riderPayable = RideTaxRates {rideFare = rates.rideFare, toll = rates.toll, parking = rates.parking, airportEntryFee = rates.airportEntryFee, cancellationFee = commissionAndTax >>= consequenceFeeTax, paymentCharge = rates.paymentCharge},
      driverDeductible = currentDriverDeductible {Tax.cancellationCommission = commissionAndTax >>= (.commissionTax)},
      platformDeductible = rideTax.platformDeductible
    }
  where
    rates = rideTax.riderPayable
    currentDriverDeductible = rideTax.driverDeductible
    commissionAndTax = row.customerCommissionAndTax

consequenceFeeTax :: DExtra.CommissionAndTax -> Maybe Tax.TaxConfig
consequenceFeeTax c =
  (\pct -> Tax.TaxConfig {tax = asPct pct, cgst = asPct <$> c.cgstPercentage, sgst = asPct <$> c.sgstPercentage, igst = asPct <$> c.igstPercentage}) <$> c.taxPercentage
  where
    asPct = fromRational . (.getHighPrecMoney)

cityRideTax :: DTC.TransporterConfig -> Tax.RideTax
cityRideTax tc = resolveTaxRates tc (Nothing :: Maybe DFP.FullFarePolicy) Nothing

data TaxableAmount = Inclusive HighPrecMoney | Exclusive HighPrecMoney

data TaxableAmounts = TaxableAmounts
  { commission :: Maybe HighPrecMoney,
    cancellationFee :: Maybe TaxableAmount,
    cancellationCommission :: Maybe HighPrecMoney,
    paymentCharge :: Maybe HighPrecMoney
  }

noTaxableAmounts :: TaxableAmounts
noTaxableAmounts = TaxableAmounts {commission = Nothing, cancellationFee = Nothing, cancellationCommission = Nothing, paymentCharge = Nothing}

resolveTaxBreakups :: Currency -> Maybe Tax.RideTax -> Maybe DFParams.FareParameters -> TaxableAmounts -> Tax.TaxBreakups
resolveTaxBreakups currency mbRideTax mbFareParams amounts =
  Tax.TaxBreakups
    { riderPayable =
        RideTaxRates
          { rideFare = stored.rideFare,
            toll = stored.toll,
            parking = stored.parking,
            airportEntryFee = stored.airportEntryFee,
            cancellationFee = maybe stored.cancellationFee cancellationFeeTax amounts.cancellationFee,
            paymentCharge = stored.paymentCharge
          },
      driverDeductible =
        Tax.DriverDeductible
          { commission = commissionTaxOf currency mbRideTax amounts.commission,
            cancellationCommission = maybe storedCancellationCommission cancellationCommissionTax amounts.cancellationCommission,
            paymentCharge = paymentChargeTax (.driverDeductible.paymentCharge)
          },
      platformDeductible = Tax.PlatformDeductible {paymentCharge = paymentChargeTax (.platformDeductible.paymentCharge)}
    }
  where
    stored = maybe (pure Nothing) fareTaxesOf mbFareParams
    storedCancellationCommission = mbFareParams >>= \fp -> Tax.mkTaxBreakup fp.customerCancellationDuesCommissionTax fp.customerCancellationDuesCommissionTaxCgst fp.customerCancellationDuesCommissionTaxSgst fp.customerCancellationDuesCommissionTaxIgst
    cancellationFeeTax taxable =
      mbRideTax >>= \rt -> case taxable of
        Inclusive gross -> Tax.taxOnInclusive currency rt.regime rt.riderPayable.cancellationFee gross
        Exclusive base -> Tax.taxOnExclusive currency rt.regime rt.riderPayable.cancellationFee base
    cancellationCommissionTax gross = mbRideTax >>= \rt -> Tax.taxOnInclusive currency rt.regime rt.driverDeductible.cancellationCommission gross
    paymentChargeTax rateOf = do
      gross <- amounts.paymentCharge
      rate <- mbRideTax >>= rateOf
      Tax.taxOnInclusive currency (Tax.chargeRegime rate) (Just rate) gross

fareTaxesOf :: DFParams.FareParameters -> Tax.FareTaxes
fareTaxesOf fp =
  RideTaxRates
    { rideFare = Tax.mkTaxBreakup fp.govtCharges fp.rideTaxCgst fp.rideTaxSgst fp.rideTaxIgst,
      toll = Tax.mkTaxBreakup fp.tollFareTax fp.tollTaxCgst fp.tollTaxSgst fp.tollTaxIgst,
      parking = Tax.mkTaxBreakup fp.parkingChargeTax fp.parkingTaxCgst fp.parkingTaxSgst fp.parkingTaxIgst,
      airportEntryFee = Tax.mkTaxBreakup fp.airportEntryFeeTax fp.airportEntryFeeTaxCgst fp.airportEntryFeeTaxSgst fp.airportEntryFeeTaxIgst,
      cancellationFee = Tax.mkTaxBreakup fp.customerCancellationDuesTax fp.cancellationTaxCgst fp.cancellationTaxSgst fp.cancellationTaxIgst,
      paymentCharge = Tax.mkTaxBreakup fp.paymentProcessingFeeTax fp.paymentChargeTaxCgst fp.paymentChargeTaxSgst fp.paymentChargeTaxIgst
    }

fareTaxTotal :: DFParams.FareParameters -> HighPrecMoney
fareTaxTotal = sum . fmap Tax.taxTotal . fareTaxesOf

regimeOf :: DTC.TransporterConfig -> Tax.Regime
regimeOf tc = fromMaybe (if isJust (gstTax tc.taxConfig.rideGst) then Tax.GST else Tax.NONE) tc.taxConfig.taxRegime

gstTax :: DTC.GstBreakup -> Maybe Tax.TaxConfig
gstTax gst =
  if totalPct > 0
    then Just Tax.TaxConfig {tax = totalPct, cgst = asPct gst.cgstPercentage, sgst = asPct gst.sgstPercentage, igst = asPct gst.igstPercentage}
    else Nothing
  where
    asPct = fmap (\fraction -> fromRational (fraction.getHighPrecMoney * 100))
    totalPct = sum (mapMaybe asPct [gst.cgstPercentage, gst.sgstPercentage, gst.igstPercentage])

bookingTax :: DRB.Booking -> DTC.TransporterConfig -> Tax.RideTax
bookingTax booking tc = fromMaybe (cityRideTax tc) booking.rideTax

bookingRideTax :: DRB.Booking -> DTC.TransporterConfig -> Maybe Tax.RideTax
bookingRideTax booking tc = Just (bookingTax booking tc)

taxPctOf :: Maybe Tax.RideTax -> (RideTaxRates (Maybe Tax.TaxConfig) -> Maybe Tax.TaxConfig) -> Maybe Double
taxPctOf mbRideTax rateOf = (\rt -> fromMaybe 0 (Tax.taxPct rt.regime (rateOf rt.riderPayable))) <$> mbRideTax

gstBreakupOf :: Maybe Tax.RideTax -> (RideTaxRates (Maybe Tax.TaxConfig) -> Maybe Tax.TaxConfig) -> DTC.GstBreakup
gstBreakupOf mbRideTax rateOf = case mbRideTax >>= \rt -> if rt.regime == Tax.GST then rateOf rt.riderPayable else Nothing of
  Just t
    | all isNothing [t.cgst, t.sgst, t.igst] -> DTC.GstBreakup {cgstPercentage = fraction (Just (t.tax / 2)), sgstPercentage = fraction (Just (t.tax / 2)), igstPercentage = Nothing}
    | otherwise -> DTC.GstBreakup {cgstPercentage = fraction t.cgst, sgstPercentage = fraction t.sgst, igstPercentage = fraction t.igst}
  Nothing -> DTC.GstBreakup {cgstPercentage = Nothing, sgstPercentage = Nothing, igstPercentage = Nothing}
  where
    fraction = fmap (\pct -> HighPrecMoney (toRational pct / 100))

splitInclusive :: Maybe Tax.RideTax -> (RideTaxRates (Maybe Tax.TaxConfig) -> Maybe Tax.TaxConfig) -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitInclusive mbRideTax rateOf = Tax.splitGrossByTaxPct (taxPctOf mbRideTax rateOf)

commissionTaxPct :: Maybe Tax.RideTax -> Maybe Double
commissionTaxPct mbRideTax = mbRideTax >>= \rt -> Tax.taxPct rt.regime rt.driverDeductible.commission

splitCommissionInclusive :: Maybe Tax.RideTax -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitCommissionInclusive = Tax.splitGrossByTaxPct . commissionTaxPct

commissionTaxOf :: Currency -> Maybe Tax.RideTax -> Maybe HighPrecMoney -> Maybe TaxBreakup
commissionTaxOf currency mbRideTax mbGross = (\gross -> fromMaybe noTax (mbRideTax >>= \rt -> Tax.taxOnInclusive currency rt.regime rt.driverDeductible.commission gross)) <$> mbGross
  where
    noTax = TaxBreakup {total = 0, cgst = Nothing, sgst = Nothing, igst = Nothing}

gstBreakupFromTax :: Maybe TaxBreakup -> DTC.GstBreakup
gstBreakupFromTax = \case
  Just t
    | t.total > 0 && all isNothing [t.cgst, t.sgst, t.igst] -> DTC.GstBreakup {cgstPercentage = Just 0.5, sgstPercentage = Just 0.5, igstPercentage = Nothing}
    | t.total > 0 -> DTC.GstBreakup {cgstPercentage = share t t.cgst, sgstPercentage = share t t.sgst, igstPercentage = share t t.igst}
  _ -> DTC.GstBreakup {cgstPercentage = Nothing, sgstPercentage = Nothing, igstPercentage = Nothing}
  where
    share t = fmap (/ t.total)

addTaxBreakup :: Maybe TaxBreakup -> Maybe TaxBreakup -> Maybe TaxBreakup
addTaxBreakup (Just a) (Just b) = Just TaxBreakup {total = a.total + b.total, cgst = addPart a.cgst b.cgst, sgst = addPart a.sgst b.sgst, igst = addPart a.igst b.igst}
  where
    addPart x y = if isNothing x && isNothing y then Nothing else Just (fromMaybe 0 x + fromMaybe 0 y)
addTaxBreakup a b = a <|> b

scaleTaxBreakup :: Currency -> Rational -> TaxBreakup -> Maybe TaxBreakup
scaleTaxBreakup currency factor t
  | scaled == 0 = Nothing
  | all isNothing [t.cgst, t.sgst, t.igst] = Just TaxBreakup {total = scaled, cgst = Nothing, sgst = Nothing, igst = Nothing}
  | otherwise = Just TaxBreakup {total = scaled, cgst = cgstPart, sgst = sgstPart, igst = igstPart}
  where
    scaled = roundAmountByCurrency' currency (HighPrecMoney (t.total.getHighPrecMoney * factor))
    weight = maybe 0 (.getHighPrecMoney)
    (cgstPart, sgstPart, igstPart) = Tax.splitGst currency (weight t.cgst, weight t.sgst, weight t.igst) scaled
