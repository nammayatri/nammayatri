{-
  BPP (Seller App / Driver-side) finance — reference-type constants + helpers.
  The ledger-posting logic that consumes these refs lives in
  Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/EndRide/Internal.hs
  (inside 'createDriverWalletTransaction').

  ┌──────────────────────────────────────────────────────────────────────────────────────┐
  │ BPP — ledger model (VAT regime). Online and cash diverge on ride-earning and         │
  │ Tips; platform-level flows (subsidy, commission, VATInput, TDS) are identical.       │
  ├──────────────────────────────────┬────────────────────────┬──────────────────────────┤
  │ Ref type                         │ Online                 │ Cash                     │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ Ride-earning                     │ 2-leg pass-through:    │ 1-leg tracking:          │
  │   (BaseRide, VATOnline/VATCash,  │   BuyerAsset →         │   BuyerControl →         │
  │    TollCharges, ParkingCharges)  │     BuyerExternal      │     OwnerControl         │
  │                                  │   BuyerExternal →      │ (Dr Asset ↑,             │
  │                                  │     OwnerLiability     │  Cr Liability ↑)         │
  │                                  │ end: BuyerAsset ↑,     │ Rider paid driver        │
  │                                  │      OwnerLiability ↑  │ directly — doesn't touch │
  │                                  │ (BPP holds cash,       │ A/R or driver wallet.    │
  │                                  │  owes driver)          │                          │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ BAP subsidy                      │ 2-leg pass-through:    │ same as online           │
  │   (Discounts,                    │   BuyerAsset →         │ (BAP actually remits     │
  │    VATAbsorbedOnDiscount)        │     BuyerExternal →    │  the subsidy to BPP in   │
  │                                  │     OwnerLiability     │  both modes; driver      │
  │                                  │                        │  wallet is credited)     │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ Commission                       │ OwnerLiability →       │ same (driver owes        │
  │   (driver pays BPP platform fee) │   SellerRevenue        │  regardless; wallet      │
  │                                  │                        │  reduces — may go -ve    │
  │                                  │                        │  for cash when wallet    │
  │                                  │                        │  credits < deductions)   │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ VATInput                         │ GovtIndirect →         │ same                     │
  │   (VAT input credit on driver's  │   OwnerLiability       │                          │
  │    taxable service; base:        │                        │                          │
  │      online: totalFare - comm    │                        │                          │
  │      cash + discount:            │                        │                          │
  │        discount - commission     │                        │                          │
  │      cash no-discount: gated off)│                        │                          │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ TDS                              │ OwnerLiability →       │ same                     │
  │   (TDSDeductionOnline/Cash)      │   GovtDirect           │                          │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ Tips                             │ BuyerAsset →           │ BuyerControl →           │
  │                                  │   OwnerLiability       │   OwnerControl           │
  │                                  │ (customer paid via     │ (tip paid directly to    │
  │                                  │  platform)             │  driver)                 │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ Payment charge — customer-borne  │ BuyerAsset →           │ BuyerControl →           │
  │   (PaymentChargePaidByCustomer,  │   BuyerExternal →      │   OwnerControl           │
  │    + its VAT; only when the      │   OwnerLiability       │ (rider handed the driver │
  │    bearer is PAYMENT_CUSTOMER)   │                        │  the grossed-up cash)    │
  ├──────────────────────────────────┼────────────────────────┼──────────────────────────┤
  │ Payment charge — deduction       │ OwnerLiability →       │ same (driver owes the    │
  │   (PGPaymentCharges + VAT;       │   SellerLiability      │  gateway fee regardless  │
  │    levied on every payment mode) │                        │  of mode; for cash +     │
  │                                  │                        │  customer bearer the     │
  │                                  │                        │  credit lands in Control │
  │                                  │                        │  so the wallet nets -P)  │
  └──────────────────────────────────┴────────────────────────┴──────────────────────────┘

  Semantics legend:
    from → to  = Dr from, Cr to (standard double-entry).
    Asset/Expense as `from` raises its balance; as `to` lowers it.
    Liability/Revenue/External as `from` lowers; as `to` raises.

  Account-role glossary (cash side):
    * 'BuyerControl'  — Asset,  BUYER counterparty. Tracks rider-side cash-flow amounts.
    * 'OwnerControl'  — Liability, ride counterparty. Tracks driver-side cash-flow.
    * No reversal hack: cash rides post a single clean leg through Control
      accounts. Recon and earnings query Control balances directly.

  GST regime (non-VAT, legacy India): ride-earning + tip behave the same as VAT;
  only the tax-component differs. Online GST is routed via BPP (
  'BuyerAsset → BuyerExternal → GovtIndirect'); cash GST flows from the driver's
  wallet ('OwnerLiability → GovtIndirect') since the driver collected it in cash
  and owes it to govt. VATInput does not apply in GST regime.

  Invoice: created inside 'createDriverWalletTransaction' (EndRide/Internal.hs) using
  FinanceM's auto-collected entry IDs — analogous to BAP's model.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Wallet
  ( walletReferenceBaseRide,
    walletReferenceGSTOnline,
    walletReferenceTollCharges,
    walletReferenceParkingCharges,
    walletReferenceTDSDeductionOnline,
    walletReferenceGSTCash,
    walletReferenceTDSDeductionCash,
    walletReferenceTDSDeductionCancellation,
    walletReferenceOverdueCancellationCharge,
    walletReferenceOverdueCancellationTax,
    walletReferenceCancellationOverdueBenefit,
    walletReferenceCancellationOverdueBenefitTax,
    walletReferenceTopup,
    walletReferencePayout,
    walletReferencePGPaymentCharges,
    walletReferencePGPayoutCharges,
    walletReferenceConnectAccountCharges,
    walletReferencePGPaymentChargesVAT,
    walletReferencePaymentChargePaidByCustomer,
    walletReferencePaymentChargeVatPaidByCustomer,
    StripeChargeFunder (..),
    recordStripeChargeLedger,
    buildDriverChargeCtx,
    paymentBearerToFunder,
    payoutBearerToFunder,
    connectBearerToFunder,
    computeStripePayoutFee,
    walletReferenceDriverCancellationCharges,
    walletReferenceCustomerCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceWalletIncentive,
    walletCreditRefs,
    getWalletAccountByOwner,
    getControlAccountByOwner,
    getWalletAndControlAccountsByOwner,
    getWalletBalanceByOwner,
    hasMinWalletBalance,
    validateWalletDebitAmount,
    getControlBalanceByOwner,
    createWalletEntryDelta,
    utcToLocalDay,
    payoutCutoffTimeUTC,
    todayRangeUTC,
    getNonRedeemableBalance,
    financeCtxFromRide,
    buildFinanceCtx,
    resolveIsOnlineFromBooking,
    walletReferenceCommissionOnline,
    walletReferenceCommissionCash,
    walletReferenceCommissionVATOnline,
    walletReferenceCommissionVATCash,
    walletReferenceCancellationCommission,
    walletReferenceCancellationCommissionVAT,
    walletReferenceVATOnline,
    walletReferenceVATCash,
    walletReferenceD2DReferral,
    walletReferenceAirportCashRecharge,
    walletReferenceAirportCashWithdrawal,
    walletReferenceAirportEntryFeeGST,
    walletReferenceAirportEntryFee,
    walletReferenceVATInput,
    walletReferenceCancellationVATInput,
    walletReferenceTips,
    walletReferenceDiscountsOnline,
    walletReferenceDiscountsCash,
    walletReferenceDeductedAtPaymentByPlatform,
    walletReferenceRideFareRefund,
    walletReferenceRideFareRefundVAT,
    walletReferenceTollRefund,
    walletReferenceTollRefundVAT,
    walletReferenceParkingRefund,
    walletReferenceParkingRefundVAT,
    walletReferenceRideFareRefundCommission,
    walletReferenceRideFareRefundCommissionVAT,
    walletReferenceCancellationFeeRefund,
    walletReferenceCancellationFeeRefundVAT,
    walletReferenceCancellationRefundCommission,
    walletReferenceCancellationRefundCommissionVAT,
    walletReferenceCancellationOverdueBenefitRefund,
    walletReferenceCancellationOverdueBenefitRefundTax,
    splitGrossByVatPct,
    getRedeemableEntryIds,
    settleWalletEntries,
    getPayoutEligibilityData,
    walletTransferFromMerchantRefs,
    computeTdsRateReason,
    computeEffectiveTdsRate,
    applyThresholdBenefit,
    selectTds,
    panAadhaarLinkTdsEnabled,
    estimateWalletDeductions,
    formatStripeAddress,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.DriverPanCard as DPanCard
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance hiding (runFinance)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.Finance.PostActions (runFinance, runPostActionsForAccount)
import SharedLogic.Finance.WalletAccount (computeTdsRateReason, estimateWalletDeductions, getControlAccountByOwner, getControlBalanceByOwner, getWalletAccountByOwner, getWalletAndControlAccountsByOwner, getWalletBalanceByOwner, hasMinWalletBalance, validateWalletDebitAmount)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Storage.Queries.FleetOwnerInformation as QFOI
import Tools.Error (MerchantPaymentMethodError (..), TransporterError (TransporterConfigNotFound))

-- Reference type constants (PascalCase, abbreviations in all caps)

walletReferenceBaseRide :: Text
walletReferenceBaseRide = "BaseRide"

walletReferenceGSTOnline :: Text
walletReferenceGSTOnline = "GSTOnline"

walletReferenceTollCharges :: Text
walletReferenceTollCharges = "TollCharges"

walletReferenceParkingCharges :: Text
walletReferenceParkingCharges = "ParkingCharges"

walletReferenceTDSDeductionOnline :: Text
walletReferenceTDSDeductionOnline = "TDSDeductionOnline"

walletReferenceGSTCash :: Text
walletReferenceGSTCash = "GSTCash"

walletReferenceTDSDeductionCash :: Text
walletReferenceTDSDeductionCash = "TDSDeductionCash"

walletReferenceTopup :: Text
walletReferenceTopup = "WalletTopup"

walletReferencePayout :: Text
walletReferencePayout = "WalletPayout"

-- Stripe charge reference types (payment / payout / connect-account maintenance).
walletReferencePGPaymentCharges :: Text
walletReferencePGPaymentCharges = "PGPaymentCharges"

walletReferencePGPayoutCharges :: Text
walletReferencePGPayoutCharges = "PGPayoutCharges"

walletReferenceConnectAccountCharges :: Text
walletReferenceConnectAccountCharges = "ConnectAccountCharges"

walletReferencePGPaymentChargesVAT :: Text
walletReferencePGPaymentChargesVAT = "PGPaymentChargesVAT"

walletReferencePaymentChargePaidByCustomer :: Text
walletReferencePaymentChargePaidByCustomer = "PaymentChargePaidByCustomer"

walletReferencePaymentChargeVatPaidByCustomer :: Text
walletReferencePaymentChargeVatPaidByCustomer = "PaymentChargeVatPaidByCustomer"

walletReferenceDriverCancellationCharges :: Text
walletReferenceDriverCancellationCharges = "DriverCancellationCharges"

walletReferenceCustomerCancellationCharges :: Text
walletReferenceCustomerCancellationCharges = "CustomerCancellationCharges"

walletReferenceCustomerCancellationGST :: Text
walletReferenceCustomerCancellationGST = "CustomerCancellationGST"

walletReferenceTDSDeductionCancellation :: Text
walletReferenceTDSDeductionCancellation = "TDSDeductionCancellation"

walletReferenceOverdueCancellationCharge :: Text
walletReferenceOverdueCancellationCharge = "OverdueCancellationCharge"

walletReferenceOverdueCancellationTax :: Text
walletReferenceOverdueCancellationTax = "OverdueCancellationTax"

walletReferenceCancellationOverdueBenefit :: Text
walletReferenceCancellationOverdueBenefit = "CancellationOverdueBenefit"

walletReferenceCancellationOverdueBenefitTax :: Text
walletReferenceCancellationOverdueBenefitTax = "CancellationOverdueBenefitTax"

walletReferenceCommissionOnline :: Text
walletReferenceCommissionOnline = "CommissionOnline"

walletReferenceCommissionCash :: Text
walletReferenceCommissionCash = "CommissionCash"

walletReferenceCommissionVATOnline :: Text
walletReferenceCommissionVATOnline = "CommissionVATOnline"

walletReferenceCommissionVATCash :: Text
walletReferenceCommissionVATCash = "CommissionVATCash"

-- Commission on a cancellation fee (no Online/Cash suffix — matches the cancellation ref family).
walletReferenceCancellationCommission :: Text
walletReferenceCancellationCommission = "CancellationCommission"

walletReferenceCancellationCommissionVAT :: Text
walletReferenceCancellationCommissionVAT = "CancellationCommissionVAT"

walletReferenceDeductedAtPaymentByPlatform :: Text
walletReferenceDeductedAtPaymentByPlatform = "DeductedAtPaymentByPlatform"

walletReferenceVATOnline :: Text
walletReferenceVATOnline = "VATOnline"

walletReferenceVATCash :: Text
walletReferenceVATCash = "VATCash"

walletReferenceVATInput :: Text
walletReferenceVATInput = "VATInput"

walletReferenceCancellationVATInput :: Text
walletReferenceCancellationVATInput = "CancellationVATInput"

walletReferenceTips :: Text
walletReferenceTips = "Tips"

-- | BAP-absorbed customer-visible discount (subsidy paid to driver by BAP).
--   Split by payment mode so reporting can separate the two flows.
walletReferenceDiscountsOnline :: Text
walletReferenceDiscountsOnline = "DiscountsOnline"

walletReferenceDiscountsCash :: Text
walletReferenceDiscountsCash = "DiscountsCash"

walletReferenceD2DReferral :: Text
walletReferenceD2DReferral = "D2DReferral"

-- | Reference type for airport booth cash recharge (idempotent by referenceId; booth operator took amount)
walletReferenceAirportCashRecharge :: Text
walletReferenceAirportCashRecharge = "AirportCashRecharge"

-- | Reference type for airport booth cash withdrawal/reversal (debit; idempotent by referenceId)
walletReferenceAirportCashWithdrawal :: Text
walletReferenceAirportCashWithdrawal = "AirportCashWithdrawal"

-- | Reference type for airport entry fee GST ledger entry at EndRide (third party GST)
walletReferenceAirportEntryFeeGST :: Text
walletReferenceAirportEntryFeeGST = "AirportEntryFeeGST"

-- | Reference type for airport entry fee (airport portion) ledger entry at EndRide (third party charges)
walletReferenceAirportEntryFee :: Text
walletReferenceAirportEntryFee = "AirportEntryFee"

walletReferenceWalletIncentive :: Text
walletReferenceWalletIncentive = "WalletIncentive"

-- Per-component refund refTypes. Same string values as the BAP side
-- (SharedLogic.Finance.RidePayment) so cap/settlement reconcile across BAP+BPP.
-- All-caps VAT matches the ride-side 'TollVAT'.
walletReferenceRideFareRefund :: Text
walletReferenceRideFareRefund = "RideFareRefund"

walletReferenceRideFareRefundVAT :: Text
walletReferenceRideFareRefundVAT = "RideFareRefundVAT"

walletReferenceTollRefund :: Text
walletReferenceTollRefund = "TollRefund"

walletReferenceTollRefundVAT :: Text
walletReferenceTollRefundVAT = "TollRefundVAT"

walletReferenceParkingRefund :: Text
walletReferenceParkingRefund = "ParkingRefund"

walletReferenceParkingRefundVAT :: Text
walletReferenceParkingRefundVAT = "ParkingRefundVAT"

-- BPP-only: the platform's commission slice on a Case-2 ride-fare refund.
walletReferenceRideFareRefundCommission :: Text
walletReferenceRideFareRefundCommission = "RideFareRefundCommission"

walletReferenceRideFareRefundCommissionVAT :: Text
walletReferenceRideFareRefundCommissionVAT = "RideFareRefundCommissionVAT"

-- Refund of a cancellation fee (driver-side legs).
walletReferenceCancellationFeeRefund :: Text
walletReferenceCancellationFeeRefund = "CancellationFeeRefund"

walletReferenceCancellationFeeRefundVAT :: Text
walletReferenceCancellationFeeRefundVAT = "CancellationFeeRefundVAT"

-- The platform's commission slice given back on a driver-deducted cancellation-fee refund.
walletReferenceCancellationRefundCommission :: Text
walletReferenceCancellationRefundCommission = "CancellationRefundCommission"

walletReferenceCancellationRefundCommissionVAT :: Text
walletReferenceCancellationRefundCommissionVAT = "CancellationRefundCommissionVAT"

-- The platform-kept overdue benefit given back on a driver-deducted cancellation-fee refund.
walletReferenceCancellationOverdueBenefitRefund :: Text
walletReferenceCancellationOverdueBenefitRefund = "CancellationOverdueBenefitRefund"

walletReferenceCancellationOverdueBenefitRefundTax :: Text
walletReferenceCancellationOverdueBenefitRefundTax = "CancellationOverdueBenefitRefundTax"

-- | Single source of truth: all wallet reference types that represent
--   redeemable credit entries (i.e. entries that increase driver wallet balance
--   and should be tracked for settlement/payout).
--   Used by: getNonRedeemableBalance, getRedeemableEntryIds, classifyEntries.
walletCreditRefs :: [Text]
walletCreditRefs =
  [ walletReferenceBaseRide,
    walletReferenceGSTOnline,
    walletReferenceGSTCash,
    walletReferenceTollCharges,
    walletReferenceParkingCharges,
    walletReferenceTDSDeductionOnline,
    walletReferenceTDSDeductionCash,
    walletReferenceTDSDeductionCancellation,
    walletReferenceOverdueCancellationCharge,
    walletReferenceOverdueCancellationTax,
    walletReferenceTopup,
    walletReferenceAirportCashRecharge,
    walletReferenceD2DReferral,
    walletReferenceCustomerCancellationCharges,
    walletReferenceDriverCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceCommissionOnline,
    walletReferenceCommissionCash,
    walletReferenceCommissionVATOnline,
    walletReferenceCommissionVATCash,
    walletReferenceCancellationCommission,
    walletReferenceCancellationCommissionVAT,
    walletReferenceWalletIncentive,
    walletReferenceVATOnline,
    walletReferenceVATCash,
    walletReferenceVATInput,
    walletReferenceCancellationVATInput,
    walletReferenceTips,
    walletReferenceDiscountsOnline,
    walletReferenceDiscountsCash,
    walletReferenceDeductedAtPaymentByPlatform,
    walletReferencePGPaymentCharges,
    walletReferencePGPaymentChargesVAT,
    walletReferencePaymentChargePaidByCustomer,
    walletReferencePaymentChargeVatPaidByCustomer,
    walletReferencePGPayoutCharges,
    walletReferenceConnectAccountCharges
  ]

-- | Reference types for entries that represent merchant-to-driver transfers
--   (amounts funded by the merchant, not by the rider's payment).
--   Used to compute the correct transferAmount for payout orders.
walletTransferFromMerchantRefs :: [Text]
walletTransferFromMerchantRefs =
  [ walletReferenceVATInput,
    walletReferenceCancellationVATInput,
    walletReferenceDiscountsOnline,
    walletReferenceDiscountsCash
  ]

-- | Split a VAT-inclusive gross into (base, vat); the rate is inclusive ("25.5" ⇒ vat = gross × 25.5/125.5).
--   Neither side is rounded: renderers derive the shown VAT % from the stored pair, and rounding
--   one side skews it (25.5 prints as 25.65). Nothing / non-positive rate ⇒ (gross, 0).
splitGrossByVatPct :: Maybe Double -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitGrossByVatPct mbPct gross = case mbPct of
  Just pct
    | pct > 0 ->
      let vat = HighPrecMoney (gross.getHighPrecMoney * (toRational pct / toRational (100 + pct)))
       in (gross - vat, vat)
  _ -> (gross, 0)

-- Time helpers (shared across getWalletTransactions, postWalletPayout, postWalletTopup)

-- | Convert a UTC time to a local Day given a timezone offset (seconds from UTC)
utcToLocalDay :: NominalDiffTime -> UTCTime -> Time.Day
utcToLocalDay timeDiff utcTime = Time.utctDay (Time.addUTCTime timeDiff utcTime)

-- | Compute the payout cutoff time in UTC.
--   Entries after this time are considered non-redeemable (recent ride earnings).
payoutCutoffTimeUTC :: NominalDiffTime -> Int -> UTCTime -> UTCTime
payoutCutoffTimeUTC timeDiff cutOffDays now =
  let localDay = utcToLocalDay timeDiff now
      cutOffDay = Time.addDays (negate (fromIntegral cutOffDays)) localDay
   in Time.addUTCTime (negate timeDiff) (Time.UTCTime cutOffDay 0)

-- | Get the UTC time range for "today" given a timezone offset.
--   Returns (startOfDayUTC, endOfDayUTC).
todayRangeUTC :: NominalDiffTime -> UTCTime -> (UTCTime, UTCTime)
todayRangeUTC timeDiff now =
  let localDay = utcToLocalDay timeDiff now
      start = Time.addUTCTime (negate timeDiff) (Time.UTCTime localDay 0)
      end = Time.addUTCTime (negate timeDiff) (Time.UTCTime localDay 86399)
   in (start, end)

-- | Calculate non-redeemable balance: sum of recent credit entries after payout cutoff.
--   Uses DB-level filtering to only fetch credits in the cutoff→now window.
getNonRedeemableBalance ::
  (BeamFlow m r) =>
  Id Account ->
  NominalDiffTime -> -- timezone offset
  Int -> -- payoutCutOffDays
  UTCTime -> -- current time
  m HighPrecMoney
getNonRedeemableBalance accountId timeDiff cutOffDays now = do
  let cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  credits <- findCreditsByAccountAfterTime accountId cutoff now
  pure $ sum $ map (.amount) credits

-- | Build a FinanceCtx from booking + ride data.
--   Resolves merchant name, shortId, address, supplier info, and TDS rate reason from DB.
--   This is the standard way to create a context for wallet operations.
buildFinanceCtx ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  SRB.Booking ->
  DRide.Ride ->
  Maybe DP.Person ->
  Maybe DPanCard.DriverPanCard ->
  Maybe DDI.DriverInformation ->
  DTC.TransporterConfig ->
  Bool -> -- isOnline (True = online/card/platform-wallet, False = cash)
  m FinanceCtx
buildFinanceCtx booking ride mbDriver mbPanCard mbDriverInfo transporterConfig isOnline = do
  let merchantId = fromMaybe booking.providerId ride.merchantId
      mid = merchantId.getId
      mocid = booking.merchantOperatingCityId.getId
      (cType, cId) = case ride.fleetOwnerId of
        Just fleetOwnerId -> (FLEET_OWNER, fleetOwnerId.getId)
        Nothing -> (DRIVER, ride.driverId.getId)
  -- Resolve merchant info
  mbMerchant <- CQM.findById merchantId
  mbMerchantOpCity <- CQMOC.findById booking.merchantOperatingCityId
  let mName = mbMerchant <&> (.name)
      mGstin = (mbMerchantOpCity >>= (.gstin)) <|> (mbMerchant >>= (.gstin))
      mVatNumber = mbMerchant >>= (.vatNumber)
      mShortId = mbMerchant <&> (.shortId.getShortId)
      address =
        mbMerchantOpCity <&> \city ->
          show city.city <> ", " <> show city.state <> ", " <> show city.country
  -- Resolve supplier info (fleet owner or driver) and detect LDC custom rate
  let configDefaultTdsRate = (.rate) <$> transporterConfig.taxConfig.defaultTdsRate
  (sName, sGSTIN, sVatNumber, sAddress, sId, hasCustomRate) <- case ride.fleetOwnerId of
    Just fleetOwnerId -> do
      mbFleetInfo <- QFOI.findByPrimaryKey (cast fleetOwnerId)
      let customRate = mbFleetInfo >>= (.tdsRate) >>= \r -> if configDefaultTdsRate == Just r then Nothing else Just r
          formattedAddress = mbFleetInfo >>= (.stripeAddress) <&> formatStripeAddress
      pure
        ( mbFleetInfo >>= (.fleetName),
          mbFleetInfo >>= (.gstNumberDec),
          mbFleetInfo >>= (.vatNumber),
          formattedAddress,
          Just fleetOwnerId.getId,
          isJust customRate
        )
    Nothing -> do
      let customRate = mbDriverInfo >>= (.tdsRate) >>= \r -> if configDefaultTdsRate == Just r then Nothing else Just r
      pure
        ( mbDriver <&> \d -> d.firstName <> maybe "" (" " <>) d.lastName,
          Nothing,
          Nothing,
          Nothing,
          Just ride.driverId.getId,
          isJust customRate
        )
  -- Resolve PAN info from already-fetched DriverPanCard
  panDecrypted <- traverse (decrypt . (.panCardNumber)) mbPanCard
  let panTypeText = mbPanCard >>= (fmap show . (.docType))
  -- Compute TDS rate reason
  let rateReason = computeTdsRateReason mbPanCard hasCustomRate
  pure
    FinanceCtx
      { merchantId = mid,
        merchantOpCityId = mocid,
        currency = booking.currency,
        isOnline = isOnline,
        counterpartyType = cType,
        counterpartyId = cId,
        concernedIndividualId = Just ride.driverId.getId,
        referenceId = booking.id.getId,
        entityReferenceId = Nothing,
        entityReferenceType = Nothing,
        merchantName = mName,
        merchantShortId = mShortId,
        issuedByAddress = address,
        supplierName = sName,
        supplierGSTIN = sGSTIN,
        supplierVatNumber = sVatNumber,
        supplierAddress = sAddress,
        merchantGstin = mGstin,
        merchantVatNumber = mVatNumber,
        supplierId = sId,
        panOfParty = panDecrypted,
        panType = panTypeText,
        tdsRateReason = rateReason,
        emitLedgerEntries = maybe True (\DTC.InvoiceConfig {emitLedgerEntries = e} -> e) transporterConfig.invoiceConfig,
        fromLocationAddress = listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city],
        issuedToName = Nothing,
        enableWalletGatedTierCheck = fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck
      }

-- | Format a Stripe Address into a single text string for supplier_address on invoices.
formatStripeAddress :: Stripe.Address -> Text
formatStripeAddress addr =
  T.intercalate ", " $ catMaybes [addr.line1, addr.line2, addr.city, addr.state, addr.postal_code, addr.country]

-- | Resolve online-vs-cash for a booking from its attached payment method.
--   Cash / BoothOnline → False; any other instrument (Card / Wallet / UPI /
--   NetBanking) → True. Returns False when no payment method is attached
--   (treated as offline). Independent of the ride-end 'forceOnlineLedger'
--   override — that override lives at the ride-end call site.
resolveIsOnlineFromBooking ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  SRB.Booking ->
  m Bool
resolveIsOnlineFromBooking booking = do
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId ->
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  pure $ case mbPaymentMethod of
    Nothing -> False
    Just paymentMethod -> case paymentMethod.paymentInstrument of
      DMPM.Cash -> False
      DMPM.BoothOnline -> False
      _ -> True

-- | Build a minimal FinanceCtx without invoice fields (for callers that
--   only need transfers, not invoices).
financeCtxFromRide :: (EncFlow m r, MonadFlow m) => SRB.Booking -> DRide.Ride -> Maybe DPanCard.DriverPanCard -> Bool -> m FinanceCtx
financeCtxFromRide booking ride mbPanCard isOnline = do
  let merchantId = fromMaybe booking.providerId ride.merchantId
      (cType, cId) = case ride.fleetOwnerId of
        Just fleetOwnerId -> (FLEET_OWNER, fleetOwnerId.getId)
        Nothing -> (DRIVER, ride.driverId.getId)
  panDecrypted <- traverse (decrypt . (.panCardNumber)) mbPanCard
  let panTypeText = mbPanCard >>= (fmap show . (.docType))
      rateReason = computeTdsRateReason mbPanCard False
  pure
    FinanceCtx
      { merchantId = merchantId.getId,
        merchantOpCityId = booking.merchantOperatingCityId.getId,
        currency = booking.currency,
        isOnline = isOnline,
        counterpartyType = cType,
        counterpartyId = cId,
        concernedIndividualId = Just ride.driverId.getId,
        referenceId = booking.id.getId,
        entityReferenceId = Nothing,
        entityReferenceType = Nothing,
        merchantName = Nothing,
        merchantShortId = Nothing,
        issuedByAddress = Nothing,
        supplierName = Nothing,
        supplierGSTIN = Nothing,
        supplierVatNumber = Nothing,
        supplierAddress = Nothing,
        merchantGstin = Nothing,
        merchantVatNumber = Nothing,
        supplierId = Nothing,
        panOfParty = panDecrypted,
        panType = panTypeText,
        tdsRateReason = rateReason,
        emitLedgerEntries = True,
        fromLocationAddress = listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city],
        issuedToName = Nothing,
        enableWalletGatedTierCheck = False -- no transporterConfig in scope here; this function has no real callers today
      }

-- Wallet entry delta (for topup/payout)

createWalletEntryDelta ::
  (BeamFlow m r, Lib.Finance.HasActorInfo m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  HighPrecMoney -> -- Delta (positive credit, negative debit)
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference type
  Text -> -- Reference ID
  Maybe Lib.Finance.Domain.Types.LedgerEntry.LedgerEntryMetadata ->
  m (Either FinanceError HighPrecMoney)
createWalletEntryDelta counterpartyType ownerId delta currency merchantId merchantOperatingCityId referenceType referenceId metadata = do
  if delta == 0
    then do
      mbBalance <- getWalletBalanceByOwner counterpartyType ownerId
      pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBalance
    else do
      let walletInput =
            AccountInput
              { accountType = Liability,
                counterpartyType = Just counterpartyType,
                counterpartyId = Just ownerId,
                subLedger = Nothing,
                currency = currency,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOperatingCityId
              }
          platformInput =
            AccountInput
              { accountType = Asset,
                counterpartyType = Just SELLER,
                counterpartyId = Just merchantId,
                subLedger = Nothing,
                currency = currency,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOperatingCityId
              }
      mbOwnerAccount <- getOrCreateAccount walletInput
      mbPlatformAccount <- getOrCreateAccount platformInput
      case (mbOwnerAccount, mbPlatformAccount) of
        (Right ownerAccount, Right platformAccount) -> do
          let concernedIndividualId =
                if counterpartyType == DRIVER
                  then Just ownerId
                  else Nothing
          let (fromAcc, toAcc, amount, eType) =
                if delta > 0
                  then (platformAccount.id, ownerAccount.id, delta, Lib.Finance.Domain.Types.LedgerEntry.Expense)
                  else (ownerAccount.id, platformAccount.id, abs delta, Lib.Finance.Domain.Types.LedgerEntry.Revenue)
          let entryInput =
                LedgerEntryInput
                  { fromAccountId = fromAcc,
                    toAccountId = toAcc,
                    concernedIndividualId = concernedIndividualId,
                    amount = amount,
                    currency = currency,
                    entryType = eType,
                    status = SETTLED,
                    referenceType = referenceType,
                    referenceId = referenceId,
                    entityReferenceId = Nothing,
                    entityReferenceType = Nothing,
                    metadata = metadata,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId,
                    settlementStatus = if delta > 0 && referenceType `elem` walletCreditRefs then Just UNSETTLED else Nothing
                  }
          entryRes <- createEntryWithBalanceUpdate entryInput
          case entryRes of
            Left err -> pure $ Left err
            Right _ -> do
              -- This function never goes through runFinance, so it can't ride the automatic
              -- PostActions dispatch. Call it explicitly here, once, for both callers.
              transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId)
              runPostActionsForAccount (fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck) (toAffectedAccount ownerAccount)
              mbBal <- getBalance ownerAccount.id
              pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBal
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err

-- Stripe charge ledger (payment / payout / connect-account) --------------------

-- | Which party funds a Stripe charge.
--   Platform → single entry BuyerAsset → SellerExpense (platform absorbs);
--   Customer → customer's grossed-up payment funds it (BuyerAsset → SellerRevenue);
--   Driver   → driver's wallet funds it (OwnerLiability → SellerRevenue).
data StripeChargeFunder = FundByPlatform | FundByCustomer | FundByDriver
  deriving (Eq, Show)

-- | Post the ledger legs for a Stripe charge, per the approved bearer model:
--     Customer/Driver: transfer_ SellerExpense SellerLiability amount  (platform expense + payable to Stripe)
--                      + funder leg: Customer → transfer BuyerAsset    SellerRevenue amount
--                                    Driver   → transfer OwnerLiability SellerRevenue amount
--     Platform:        transfer BuyerAsset SellerExpense amount        (platform absorbs; single entry)
--   The ctx MUST carry a DRIVER/FLEET_OWNER counterparty so the driver funding leg
--   hits the driver's OwnerLiability account (Seller*/Buyer* roles are hardwired).
recordStripeChargeLedger ::
  (MonadFlow m, BeamFlow m r, Lib.Finance.HasActorInfo m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  FinanceCtx ->
  StripeChargeFunder ->
  HighPrecMoney ->
  Text ->
  m (Either FinanceError ())
recordStripeChargeLedger ctx funder amount refType
  | amount <= 0 = pure (Right ())
  | otherwise = do
    result <- runFinance ctx $ case funder of
      FundByPlatform -> void $ transfer BuyerAsset SellerExpense amount refType Nothing
      FundByCustomer -> do
        -- owner liablity to seller liablity
        transfer_ SellerExpense SellerLiability amount refType
        void $ transfer BuyerAsset SellerRevenue amount refType Nothing
      FundByDriver -> do
        transfer_ SellerExpense SellerLiability amount refType
        void $ transfer OwnerLiability SellerRevenue amount refType Nothing
    pure (void result)

-- | Minimal FinanceCtx for posting a driver-side Stripe charge (payout / connect),
--   where no booking/ride is in scope. Counterparty is the driver (or fleet owner),
--   so OwnerLiability resolves to their wallet.
buildDriverChargeCtx ::
  CounterpartyType -> -- DRIVER or FLEET_OWNER
  Text -> -- owner (driver / fleet owner) id
  Text -> -- merchant id
  Text -> -- merchant operating city id
  Currency ->
  Text -> -- reference id (e.g. payout order id / period key)
  Bool -> -- caller-resolved driverWalletConfig.enableWalletGatedTierCheck
  FinanceCtx
buildDriverChargeCtx counterpartyType ownerId merchantId merchantOperatingCityId currency referenceId walletGateEnabled =
  FinanceCtx
    { merchantId = merchantId,
      merchantOpCityId = merchantOperatingCityId,
      currency = currency,
      isOnline = True,
      counterpartyType = counterpartyType,
      counterpartyId = ownerId,
      concernedIndividualId = if counterpartyType == DRIVER then Just ownerId else Nothing,
      referenceId = referenceId,
      entityReferenceId = Nothing,
      entityReferenceType = Nothing,
      merchantName = Nothing,
      merchantShortId = Nothing,
      issuedByAddress = Nothing,
      supplierName = Nothing,
      supplierGSTIN = Nothing,
      supplierVatNumber = Nothing,
      supplierAddress = Nothing,
      merchantGstin = Nothing,
      merchantVatNumber = Nothing,
      supplierId = Nothing,
      panOfParty = Nothing,
      panType = Nothing,
      tdsRateReason = Nothing,
      emitLedgerEntries = True,
      fromLocationAddress = Nothing,
      issuedToName = Nothing,
      enableWalletGatedTierCheck = walletGateEnabled
    }

-- | Stripe payout charge Q = fixedFee + percentageRate% * amount (new model),
--   falling back to the legacy single-mode feeType/feeValue when the new fields
--   are unset. Capped at the payout amount.
computeStripePayoutFee :: DTC.PayoutFeeConfig -> HighPrecMoney -> HighPrecMoney
computeStripePayoutFee cfg amount =
  let usesNewModel = isJust cfg.fixedFee || isJust cfg.percentageRate
      fee =
        if usesNewModel
          then fromMaybe 0 cfg.fixedFee + amount * realToFrac (fromMaybe 0 cfg.percentageRate) / 100
          else case cfg.feeType of
            DTC.PERCENTAGE -> amount * cfg.feeValue / 100
            DTC.FIXED -> cfg.feeValue
   in min fee amount

-- Bearer → funder mappings (kept here so callers never touch the raw
-- constructors, which collide across the three bearer enums).

paymentBearerToFunder :: DTC.PaymentChargeBearer -> StripeChargeFunder
paymentBearerToFunder bearer = case bearer of
  DTC.PAYMENT_PLATFORM -> FundByPlatform
  DTC.PAYMENT_CUSTOMER -> FundByCustomer
  DTC.PAYMENT_DRIVER -> FundByDriver

payoutBearerToFunder :: DTC.PayoutChargeBearer -> StripeChargeFunder
payoutBearerToFunder bearer = case bearer of
  DTC.PLATFORM_BEARER -> FundByPlatform
  DTC.DRIVER_BEARER -> FundByDriver

connectBearerToFunder :: DTC.ConnectChargeBearer -> StripeChargeFunder
connectBearerToFunder bearer = case bearer of
  DTC.CONNECT_PLATFORM -> FundByPlatform
  DTC.CONNECT_DRIVER -> FundByDriver

-- | Get all unsettled redeemable wallet entry IDs (credits + debits before cutoff).
--   Uses DB-level filtering for efficiency.
getRedeemableEntryIds ::
  (BeamFlow m r) =>
  Id Account ->
  UTCTime -> -- payout cutoff time
  m [Id LedgerEntry]
getRedeemableEntryIds accountId cutoff = do
  entries <- findUnsettledByAccountBeforeTime accountId cutoff
  pure $ map (.id) entries

-- | Fetch payout eligibility data using two efficient DB-level queries:
--   (1) non-redeemable balance: sum of credits after cutoff (DB-filtered)
--   (2) redeemable entry IDs: unsettled credits + debits before cutoff (DB-filtered)
--   (3) merchant transfer amount: sum of VAT input + discount entries from unsettled entries
--   This avoids fetching all entries into Haskell memory.
getPayoutEligibilityData ::
  (BeamFlow m r) =>
  Id Account ->
  UTCTime -> -- payout cutoff time
  UTCTime -> -- current time (upper bound)

  -- | (nonRedeemableBalance, redeemableEntryIds, merchantTransferAmount)
  m (HighPrecMoney, [Id LedgerEntry], HighPrecMoney)
getPayoutEligibilityData accountId cutoff now = do
  -- Query 1: credits after cutoff (for non-redeemable balance)
  creditsAfterCutoff <- findCreditsByAccountAfterTime accountId cutoff now
  let nonRedeemableBalance = sum $ map (.amount) creditsAfterCutoff
  -- Query 2: unsettled entries before cutoff (for redeemable IDs + transfer amount)
  unsettledBeforeCutoff <- findUnsettledByAccountBeforeTime accountId cutoff
  let redeemableIds = map (.id) unsettledBeforeCutoff
      -- Transfer amount: sum of credits (toAccountId == accountId) with merchant-transfer ref types.
      -- These represent amounts funded by the merchant (VAT input, discounts), not from the rider's payment.
      merchantTransferAmount =
        sum
          [ e.amount
            | e <- unsettledBeforeCutoff,
              e.toAccountId == accountId,
              e.referenceType `elem` walletTransferFromMerchantRefs
          ]
  pure (nonRedeemableBalance, redeemableIds, merchantTransferAmount)

-- | Mark a list of wallet ledger entries as paid out.
--   Called by the payout webhook handler after successful disbursement.
settleWalletEntries ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  [Id LedgerEntry] -> -- entry IDs to settle
  Text -> -- PayoutRequest ID
  m ()
settleWalletEntries entryIds payoutRequestId =
  markEntriesAsPaidOut entryIds payoutRequestId

-- | True when the merchant has enabled PAN-Aadhaar-link based TDS (the cohort
-- model). Keyed off the cohort config being present (individualNotLinked).
panAadhaarLinkTdsEnabled :: DTC.TaxConfig -> Bool
panAadhaarLinkTdsEnabled taxConfig = isJust taxConfig.individualNotLinked

selectTds ::
  Maybe DPanCard.DriverPanCard ->
  DTC.TaxConfig ->
  Maybe DTC.TdsConfig
selectTds mbPanCard taxConfig
  | not (panAadhaarLinkTdsEnabled taxConfig) = Nothing
  | otherwise =
    let hasValidPan = maybe False (\pan -> pan.verificationStatus == Documents.VALID) mbPanCard
        isBusiness = (mbPanCard >>= (.docType)) == Just DPanCard.BUSINESS
        isPanLinkedToAadhaar =
          maybe False (\pan -> pan.panAadhaarLinkage == Just DPanCard.PAN_AADHAAR_LINKED) mbPanCard
     in if not hasValidPan
          then Just taxConfig.invalidPanTdsRate
          else
            if isBusiness
              then taxConfig.businessTds
              else
                if isPanLinkedToAadhaar
                  then taxConfig.individualLinked
                  else taxConfig.individualNotLinked

computeEffectiveTdsRate ::
  Maybe DPanCard.DriverPanCard -> -- PAN card info
  Maybe Double -> -- materialized TDS rate from driverInfo.tdsRate / fleetOwnerInfo.tdsRate
  DTC.TaxConfig -> -- merchant tax config
  Maybe Double -- effective rate
computeEffectiveTdsRate mbPanCard mbCustomRate taxConfig =
  case selectTds mbPanCard taxConfig of
    -- PAN-Aadhaar-link TDS: prefer the materialized rate, fall back to the
    -- cohort-selected rate.
    Just tds -> Just (fromMaybe tds.rate mbCustomRate)
    Nothing -> legacyTdsRate
  where
    hasValidPan = maybe False (\pan -> pan.verificationStatus == Documents.VALID) mbPanCard
    -- Legacy TDS (merchant hasn't enabled PAN-Aadhaar-link TDS): valid PAN →
    -- defaultTdsRate.rate (or custom override); invalid PAN → invalidPanTdsRate.rate.
    legacyTdsRate =
      if hasValidPan
        then mbCustomRate <|> ((.rate) <$> taxConfig.defaultTdsRate)
        else Just taxConfig.invalidPanTdsRate.rate

applyThresholdBenefit ::
  DTC.TaxConfig ->
  Maybe HighPrecMoney ->
  Maybe DPanCard.DriverPanCard ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney
applyThresholdBenefit taxConfig mbCumulative mbPanCard currentBase rawAmount =
  case (selectTds mbPanCard taxConfig, mbCumulative) of
    (Nothing, _) -> rawAmount
    (_, Nothing) -> rawAmount
    (Just tds, Just cumulative) -> case tds.thresholdAmount of
      Nothing -> rawAmount
      Just thresh ->
        if cumulative + currentBase <= thresh
          then 0
          else rawAmount
