module SharedLogic.RideFinance
  ( fareParamsBreakup,
    FinanceAdjustments (..),
    rideFinanceOf,
    quoteDriverNet,
    finalRideFinance,
    estimatedRideFinance,
    sumDriverNetBeforeTds,
    rideDriverNetBeforeTds,
    rideCashOnlineSplit,
    earningsClubPolicy,
    earningsPolicy,
    earningsLayout,
    walletSummaryClubPolicy,
    invoiceClubPolicy,
    taxRegime,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import qualified RideFinance.Calculations.Build as RF
import qualified RideFinance.Calculations.Totals as RFT
import qualified RideFinance.Core.Projection as RFP
import qualified RideFinance.Core.Types as RF
import RideFinance.Display.Club (Keys (..), noKeys)
import qualified RideFinance.Display.Config as RFC
import qualified RideFinance.Display.Present as RFPresent
import qualified SharedLogic.FareCalculator as FC

fareParamsBreakup :: DFP.FareParameters -> RFP.ProjectFareParamsBreakup
fareParamsBreakup fp = fromMaybe singleRideFareBreakup (FC.projectFareParamsBreakup fp)
  where
    edcCollectsParking = SL.edcCollectsParking fp.fareSettlementType
    tollExclusive = fromMaybe 0 fp.tollCharges
    tollTax = fromMaybe 0 fp.tollFareTax
    parkingExclusive = if edcCollectsParking then 0 else fromMaybe 0 fp.parkingCharge
    parkingTax = if edcCollectsParking then 0 else fromMaybe 0 fp.parkingChargeTax
    carriedCancellationDues = fromMaybe 0 fp.customerCancellationDues
    paymentChargeExclusive = fromMaybe 0 fp.paymentProcessingFee
    paymentChargeTax = fromMaybe 0 fp.paymentProcessingFeeVat
    rideTax = fromMaybe 0 fp.govtCharges
    singleRideFareBreakup =
      RFP.ProjectFareParamsBreakup
        { discountApplicableRideFareTaxExclusive = FC.fareSum fp Nothing - rideTax - tollExclusive - tollTax - parkingExclusive - parkingTax - carriedCancellationDues - paymentChargeExclusive - paymentChargeTax,
          discountApplicableRideFareTax = rideTax,
          discountApplicableRideFareUntaxed = 0,
          nonDiscountApplicableRideFareTaxExclusive = 0,
          nonDiscountApplicableRideFareTax = 0,
          nonDiscountApplicableRideFareUntaxed = carriedCancellationDues,
          tollFareTaxExclusive = tollExclusive,
          tollFareTax = tollTax,
          cancellationFeeTaxExclusive = 0,
          cancellationTax = 0,
          parkingChargeTaxExclusive = parkingExclusive,
          parkingChargeTax = parkingTax,
          paymentChargeTaxExclusive = paymentChargeExclusive,
          paymentChargeTax = paymentChargeTax
        }

taxRegime :: DFP.FareParameters -> RF.TaxRegime
taxRegime fp = if fp.isVatTaxType == Just True then RF.VAT else RF.GST

fareSettlement :: RF.SettlementType -> DFP.FareParameters -> RF.Settlement
fareSettlement paidVia fp =
  RF.Settlement
    { settlementType = paidVia,
      collectedAtBooth = maybe Set.empty boothCollects fp.fareSettlementType
    }
  where
    boothCollects = \case
      SL.FullPayment -> Set.fromList RFC.allComponents
      fareSettlementType ->
        Set.fromList $
          [RF.COMMISSION | SL.commissionCollectedAtBooth (Just fareSettlementType)]
            <> [RF.PARKING_CHARGE | SL.edcCollectsParking (Just fareSettlementType)]

settlementTypeOf :: Maybe DMPM.PaymentInstrument -> RF.SettlementType
settlementTypeOf = \case
  Just DMPM.Cash -> RF.Cash
  _ -> RF.Online

storedPair :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> RF.TaxedAmount
storedPair mbTaxExclusive mbTax = RF.TaxedAmount {taxExclusive = fromMaybe 0 mbTaxExclusive, tax = fromMaybe 0 mbTax}

pricedPaymentCharge :: DFP.FareParameters -> Maybe RF.ChargeBearer -> RF.TaxedAmount
pricedPaymentCharge fp = \case
  Just RF.PAYMENT_DRIVER -> RF.TaxedAmount {taxExclusive = fromMaybe 0 fp.driverPaymentChargeTaxExclusive, tax = fromMaybe 0 fp.driverPaymentChargeTax}
  Just RF.PAYMENT_CUSTOMER -> RF.TaxedAmount {taxExclusive = fromMaybe 0 fp.paymentProcessingFee, tax = fromMaybe 0 fp.paymentProcessingFeeVat}
  _ -> RF.TaxedAmount {taxExclusive = 0, tax = 0}

splitAtPricedRatio :: RF.TaxedAmount -> HighPrecMoney -> RF.TaxedAmount
splitAtPricedRatio priced leviedGross
  | pricedGross > 0 =
    let leviedTax = HighPrecMoney (leviedGross.getHighPrecMoney * priced.tax.getHighPrecMoney / pricedGross.getHighPrecMoney)
     in RF.TaxedAmount {taxExclusive = leviedGross - leviedTax, tax = leviedTax}
  | otherwise = RF.TaxedAmount {taxExclusive = leviedGross, tax = 0}
  where
    pricedGross = priced.taxExclusive + priced.tax

fareMemos :: DFP.FareParameters -> [(RF.MemoItem, HighPrecMoney)]
fareMemos fp =
  [ (RF.CARRIED_CANCELLATION_DUES, fromMaybe 0 fp.customerCancellationDues),
    (RF.AIRPORT_CONVENIENCE_FEE, fromMaybe 0 fp.airportConvenienceFee),
    (RF.SERVICE_CHARGE, fromMaybe 0 fp.serviceCharge)
  ]

fareParamsChargeBearer :: DFP.FareParameters -> Maybe RF.ChargeBearer
fareParamsChargeBearer fp
  | storedGross fp.driverPaymentChargeTaxExclusive fp.driverPaymentChargeTax > 0 = Just RF.PAYMENT_DRIVER
  | storedGross fp.paymentProcessingFee fp.paymentProcessingFeeVat > 0 = Just RF.PAYMENT_CUSTOMER
  | otherwise = Nothing
  where
    storedGross mbTaxExclusive mbTax = fromMaybe 0 mbTaxExclusive + fromMaybe 0 mbTax

data LeviedPaymentCharge = LeviedPaymentCharge
  { bearer :: Maybe RF.ChargeBearer,
    gross :: HighPrecMoney
  }

data FinanceAdjustments = FinanceAdjustments
  { discount :: HighPrecMoney,
    tip :: HighPrecMoney,
    leviedPaymentCharge :: Maybe LeviedPaymentCharge
  }

rideFinanceOf :: Currency -> RF.SettlementType -> DFP.FareParameters -> FinanceAdjustments -> RF.RideFinance
rideFinanceOf currency settlementType fp adjustments =
  RF.fromProjection
    RF.RideFinanceInputs
      { currency = currency,
        regime = taxRegime fp,
        breakup = fareParamsBreakup fp,
        requestedDiscount = adjustments.discount,
        tip = adjustments.tip,
        commission = storedPair fp.commissionTaxExclusive fp.commissionTax,
        cancellationCommission = storedPair fp.cancellationCommissionTaxExclusive fp.cancellationCommissionTax,
        chargeBearer = chargeBearer,
        paymentCharge = pricedCharge,
        leviedPaymentCharge = splitAtPricedRatio pricedCharge . (.gross) <$> adjustments.leviedPaymentCharge,
        memos = fareMemos fp,
        settlement = fareSettlement settlementType fp,
        tds = fp.tdsAmount
      }
  where
    chargeBearer = (adjustments.leviedPaymentCharge >>= (.bearer)) <|> fareParamsChargeBearer fp
    pricedCharge = pricedPaymentCharge fp chargeBearer

quoteDriverNet :: Currency -> Maybe DMPM.PaymentInstrument -> Maybe HighPrecMoney -> DFP.FareParameters -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
quoteDriverNet currency paymentInstrument mbDiscountAmount fp offerAdjustment = (RFT.driverNetBeforeTds quoteFinance + offerAdjustment, RFT.taxRemitted quoteFinance)
  where
    quoteFinance =
      rideFinanceOf currency (settlementTypeOf paymentInstrument) fp $
        FinanceAdjustments
          { discount = fromMaybe 0 mbDiscountAmount,
            tip = 0,
            leviedPaymentCharge = Nothing
          }

finalRideFinance :: DRB.Booking -> DRide.Ride -> DFP.FareParameters -> RF.RideFinance
finalRideFinance booking ride fp =
  rideFinanceOf ride.currency (settlementTypeOf booking.paymentInstrument) fp $
    FinanceAdjustments
      { discount = fromMaybe 0 ride.discountAmount,
        tip = fromMaybe 0 ride.tipAmount,
        leviedPaymentCharge = leviedPaymentChargeOf (ride.paymentChargeBearer <|> booking.paymentChargeBearer) (ride.paymentCharge <|> booking.paymentCharge)
      }

estimatedRideFinance :: DRB.Booking -> RF.RideFinance
estimatedRideFinance booking =
  rideFinanceOf booking.currency (settlementTypeOf booking.paymentInstrument) booking.fareParams $
    FinanceAdjustments
      { discount = fromMaybe 0 booking.discountAmount,
        tip = 0,
        leviedPaymentCharge = leviedPaymentChargeOf booking.paymentChargeBearer booking.paymentCharge
      }

leviedPaymentChargeOf :: Maybe Text -> Maybe HighPrecMoney -> Maybe LeviedPaymentCharge
leviedPaymentChargeOf mbBearer mbGross =
  mbGross <&> \leviedGross -> LeviedPaymentCharge {bearer = readMaybe . T.unpack =<< mbBearer, gross = leviedGross}

rideDriverNetBeforeTds :: DRB.Booking -> DRide.Ride -> DFP.FareParameters -> HighPrecMoney
rideDriverNetBeforeTds booking ride fp = fromMaybe (RFT.driverNetBeforeTds (finalRideFinance booking ride fp)) ride.driverNetBeforeTds

rideCashOnlineSplit :: DRB.Booking -> DRide.Ride -> DFP.FareParameters -> RFT.CashOnlineSplit
rideCashOnlineSplit booking ride fp
  | isJust ride.driverNetBeforeTds = RFT.CashOnlineSplit {collectInCash = ride.amountCollectedInCash, settledOnline = ride.amountSettledOnline}
  | otherwise = RFT.cashOnlineSplit (finalRideFinance booking ride fp)

sumDriverNetBeforeTds :: [DRide.Ride] -> HighPrecMoney
sumDriverNetBeforeTds = sum . map (\ride -> fromMaybe (fromMaybe 0 ride.fare) ride.driverNetBeforeTds)

earningsClubPolicy :: Maybe DTC.TransporterConfig -> RF.ClubPolicy
earningsClubPolicy mbTransporterConfig = RFC.clubPolicy (mbTransporterConfig >>= (\tc -> tc.driverWalletConfig.clubTaxInEarnings))

earningsPolicy :: Maybe DTC.TransporterConfig -> Bool -> RF.ClubPolicy
earningsPolicy mbTransporterConfig isFinal
  | isFinal = earningsClubPolicy mbTransporterConfig
  | otherwise = earningsClubPolicy mbTransporterConfig <> Set.fromList (filter (/= RF.PAYMENT_CHARGE) RFC.allComponents)

earningsLayout :: RFPresent.EarningsLayout
earningsLayout =
  RFPresent.EarningsLayout
    { componentKeys = \case
        RF.RIDE_FARE -> noKeys {taxKeys = ["RIDE_FARE_VAT"]}
        RF.TOLL_FARE -> noKeys {taxKeys = ["TOLL_VAT"]}
        RF.PARKING_CHARGE -> noKeys {taxKeys = ["PARKING_VAT"]}
        RF.COMMISSION -> Keys {taxExclusiveKeys = ["COMMISSION"], taxKeys = ["COMMISSION_VAT"], clubbedKey = Just "COMMISSION"}
        RF.PAYMENT_CHARGE -> Keys {taxExclusiveKeys = ["PAYMENT_CHARGE"], taxKeys = ["PAYMENT_CHARGE_VAT"], clubbedKey = Just "PAYMENT_CHARGE"}
        _ -> noKeys,
      customerPaidCode = "FARE_PAID_BY_CUSTOMER",
      discountCode = "DISCOUNT",
      tipCode = "TIPS",
      paymentChargeCollectedCode = "PAYMENT_CHARGE_PAID_BY_CUSTOMER",
      memoCode = \case
        RF.CARRIED_CANCELLATION_DUES -> "CUSTOMER_CANCELLATION_CHARGE"
        RF.AIRPORT_CONVENIENCE_FEE -> "AIRPORT_CONVENIENCE_FEE"
        RF.SERVICE_CHARGE -> "SERVICE_CHARGE"
    }

walletSummaryClubPolicy :: DTC.TransporterConfig -> RF.ClubPolicy
walletSummaryClubPolicy transporterConfig = RFC.clubPolicy transporterConfig.driverWalletConfig.clubTaxInWalletSummary

invoiceClubPolicy :: Maybe DTC.TransporterConfig -> Text -> Text -> RF.ClubPolicy
invoiceClubPolicy mbTransporterConfig = RFC.invoiceClubPolicy (fromMaybe [] (mbTransporterConfig >>= (.invoiceConfig) >>= (.vatClubRules)))
