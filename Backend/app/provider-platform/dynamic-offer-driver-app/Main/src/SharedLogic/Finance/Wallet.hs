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
    walletReferenceDriverCancellationCharges,
    walletReferenceCustomerCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceWalletIncentive,
    walletCreditRefs,
    getWalletAccountByOwner,
    getControlAccountByOwner,
    getWalletAndControlAccountsByOwner,
    getWalletBalanceByOwner,
    getControlBalanceByOwner,
    createWalletEntryDelta,
    utcToLocalDay,
    payoutCutoffTimeUTC,
    todayRangeUTC,
    getNonRedeemableBalance,
    computeGstBreakdown,
    computeGstBreakdownByPlace,
    computeGstBreakdownGSTIN,
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
    walletReferenceStatutoryHold,
    createWalletHold,
    findPendingWalletHoldByReference,
    getPendingWalletHoldAmountByReference,
    voidWalletHoldByReference,
    getWalletHoldBalanceByOwner,
    getWalletAvailableBalanceByOwner,
    makeWalletRunningBalanceLockKey,
    addWalletOfferHold,
    removeWalletOfferHold,
    getWalletOfferHoldTotal,
    getWalletOfferHoldAmount,
    addPrepaidOfferHold,
    removePrepaidOfferHold,
    getPrepaidOfferHoldTotal,
    getPrepaidOfferHoldAmount,
    addOfferHoldsForSearchTry,
    getTotalWalletHoldBalance,
    removeOfferHolds,
    getWalletOfferHoldTotalExcluding,
    getPrepaidOfferHoldTotalExcluding,
    estimateOfferDeductions,
    reserveWalletForCashRide,
    applyFareRecomputeBuffer,
    cashWalletCheckEnabled,
    shouldCheckCashWallet,
    estimateBufferedStatutoryDeductions,
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
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.OfferHold (addOfferHoldAtKey, getOfferHoldAmountAtKey, getOfferHoldTotalAtKey, removeOfferHoldAtKey)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import Storage.Queries.FleetOwnerInformation as QFOI
import Tools.Error (MerchantPaymentMethodError (..))

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
    walletReferenceDeductedAtPaymentByPlatform
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

-- Account helpers (these are still needed for non-FinanceM callers like balance queries)

getWalletAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getWalletAccountByOwner counterpartyType ownerId =
  findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Liability

-- | Returns the driver's Control (cash-earnings memo) account, if any. Distinct
--   from the Liability wallet account — Control tracks cumulative cash ride
--   earnings (direct rider → driver), while Liability tracks what the platform
--   actually owes the driver.
getControlAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getControlAccountByOwner counterpartyType ownerId =
  findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Control

-- | Fetch both Liability (real wallet) and Control (cash-earnings memo)
--   accounts for an owner. Used by the driver wallet transactions feed which
--   merges entries across both so cash rides surface alongside online earnings.
getWalletAndControlAccountsByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe Account, Maybe Account)
getWalletAndControlAccountsByOwner counterpartyType ownerId = do
  mbLiability <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Liability
  mbControl <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Control
  pure (mbLiability, mbControl)

getWalletBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

-- | Balance of the Control (cash-earnings memo) account.
getControlBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getControlBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getControlAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

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
      mGstin = mbMerchant >>= (.gstin)
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
        issuedToName = Nothing
      }

-- | Pure helper to compute TDS rate reason from PAN card data and LDC status.
computeTdsRateReason :: Maybe DPanCard.DriverPanCard -> Bool -> Maybe TdsRateReason
computeTdsRateReason mbPanCard hasCustomRate =
  let hasValidPan = maybe False (\pan -> pan.verificationStatus == Documents.VALID) mbPanCard
      panAadhaarLinked = maybe False (\pan -> pan.panAadhaarLinkage == Just DPanCard.PAN_AADHAAR_LINKED) mbPanCard
   in Just $
        if not hasValidPan
          then NO_PAN
          else
            if hasCustomRate
              then LDC_CERTIFICATE
              else
                if panAadhaarLinked
                  then PAN_AADHAR_LINKAGE
                  else PAN

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
        issuedToName = Nothing
      }

-- Wallet entry delta (for topup/payout)

createWalletEntryDelta ::
  (BeamFlow m r, Lib.Finance.HasActorInfo m r) =>
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
              mbBal <- getBalance ownerAccount.id
              pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBal
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err

walletReferenceStatutoryHold :: Text
walletReferenceStatutoryHold = "StatutoryDeductionHold"

makeWalletRunningBalanceLockKey :: Text -> Text
makeWalletRunningBalanceLockKey personId = "WalletRunningBalanceLockKey:" <> personId

applyFareRecomputeBuffer :: DTC.DriverWalletConfig -> HighPrecMoney -> HighPrecMoney
applyFareRecomputeBuffer dwc fare =
  fare + fare * realToFrac (fromMaybe 0 dwc.fareRecomputeBufferPercent) / 100 + fromMaybe 0 dwc.fareRecomputeBufferAmount

cashWalletCheckEnabled :: DTC.DriverWalletConfig -> Bool
cashWalletCheckEnabled dwc = dwc.enableDriverWallet && isJust dwc.minWalletAmountForCashRides

estimateBufferedStatutoryDeductions :: DTC.DriverWalletConfig -> DTC.TaxConfig -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney
estimateBufferedStatutoryDeductions dwc taxConfig mbFare govtCharges_ tollCharges_ parkingCharge_ =
  case mbFare of
    Nothing -> 0
    Just fare ->
      let bufferedFare = applyFareRecomputeBuffer dwc fare
          fareScale = if fare > 0 then bufferedFare.getHighPrecMoney / fare.getHighPrecMoney else 1
          gstAmount = HighPrecMoney ((fromMaybe 0 govtCharges_).getHighPrecMoney * fareScale)
          tollAmount = fromMaybe 0 tollCharges_
          parkingAmount = fromMaybe 0 parkingCharge_
          baseFare = max 0 (bufferedFare - gstAmount - tollAmount - parkingAmount)
          tdsRate = Just taxConfig.invalidPanTdsRate.rate
       in gstAmount + estimateWalletDeductions tdsRate baseFare

shouldCheckCashWallet :: Maybe DMPM.PaymentInstrument -> Bool
shouldCheckCashWallet = \case
  Nothing -> True
  Just DMPM.Cash -> True
  Just DMPM.BoothOnline -> True
  _ -> False

createWalletHold ::
  (BeamFlow m r, Lib.Finance.HasActorInfo m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  HighPrecMoney ->
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference ID (rideId / bookingId)
  Maybe Text -> -- Concerned driver ID: the individual driver the hold is for, even when the wallet is the fleet's
  Maybe Lib.Finance.Domain.Types.LedgerEntry.LedgerEntryMetadata ->
  m (Either FinanceError ())
createWalletHold counterpartyType ownerId amount currency merchantId merchantOperatingCityId referenceId mbConcernedDriverId metadata = do
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
      mbExistingHold <- findPendingWalletHoldByReference ownerAccount.id referenceId
      case mbExistingHold of
        Just _ -> pure $ Right ()
        Nothing -> do
          let entryInput =
                LedgerEntryInput
                  { fromAccountId = ownerAccount.id,
                    toAccountId = platformAccount.id,
                    concernedIndividualId = mbConcernedDriverId <|> (if counterpartyType == DRIVER then Just ownerId else Nothing),
                    amount = amount,
                    currency = currency,
                    entryType = Lib.Finance.Domain.Types.LedgerEntry.Revenue,
                    status = PENDING,
                    referenceType = walletReferenceStatutoryHold,
                    referenceId = referenceId,
                    entityReferenceId = Nothing,
                    entityReferenceType = Nothing,
                    metadata = metadata,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId,
                    settlementStatus = Nothing
                  }
          entryRes <- createEntry entryInput
          pure $ void entryRes
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

findPendingWalletHoldByReference ::
  (BeamFlow m r) =>
  Id Account ->
  Text -> -- Reference ID
  m (Maybe LedgerEntry)
findPendingWalletHoldByReference ownerAccountId referenceId = do
  entries <- getEntriesByReference walletReferenceStatutoryHold referenceId
  pure $ find (\entry -> entry.fromAccountId == ownerAccountId && entry.status == PENDING) entries

getPendingWalletHoldAmountByReference ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  Text -> -- Reference ID
  m HighPrecMoney
getPendingWalletHoldAmountByReference counterpartyType ownerId referenceId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  case mbAcc of
    Nothing -> pure 0
    Just acc -> maybe 0 (.amount) <$> findPendingWalletHoldByReference acc.id referenceId

voidWalletHoldByReference ::
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  Text -> -- Reference ID
  Text -> -- Reason
  m ()
voidWalletHoldByReference counterpartyType ownerId referenceId reason = do
  mbOwnerAccount <- getWalletAccountByOwner counterpartyType ownerId
  case mbOwnerAccount of
    Nothing -> pure ()
    Just ownerAccount -> do
      entries <- getEntriesByReference walletReferenceStatutoryHold referenceId
      let pendingEntries = filter (\entry -> entry.fromAccountId == ownerAccount.id && entry.status == PENDING) entries
      forM_ pendingEntries $ \entry -> voidEntry entry.id reason

getWalletHoldBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m HighPrecMoney
getWalletHoldBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  case mbAcc of
    Nothing -> pure 0
    Just acc -> do
      entries <- getEntriesByFromAccountStatusAndReferenceType acc.id PENDING walletReferenceStatutoryHold
      pure $ sum $ map (.amount) entries

makeWalletOfferHoldsKey :: Text -> Text
makeWalletOfferHoldsKey ownerId = "WalletOfferHolds:" <> ownerId

makePrepaidOfferHoldsKey :: Text -> Text
makePrepaidOfferHoldsKey ownerId = "PrepaidOfferHolds:" <> ownerId

addWalletOfferHold :: (CacheFlow m r, MonadFlow m) => Text -> Text -> HighPrecMoney -> UTCTime -> m ()
addWalletOfferHold = addOfferHoldAtKey . makeWalletOfferHoldsKey

removeWalletOfferHold :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m ()
removeWalletOfferHold = removeOfferHoldAtKey . makeWalletOfferHoldsKey

getWalletOfferHoldTotal :: (CacheFlow m r, MonadFlow m) => Text -> m HighPrecMoney
getWalletOfferHoldTotal = getOfferHoldTotalAtKey . makeWalletOfferHoldsKey

getWalletOfferHoldAmount :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m HighPrecMoney
getWalletOfferHoldAmount = getOfferHoldAmountAtKey . makeWalletOfferHoldsKey

addPrepaidOfferHold :: (CacheFlow m r, MonadFlow m) => Text -> Text -> HighPrecMoney -> UTCTime -> m ()
addPrepaidOfferHold = addOfferHoldAtKey . makePrepaidOfferHoldsKey

removePrepaidOfferHold :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m ()
removePrepaidOfferHold = removeOfferHoldAtKey . makePrepaidOfferHoldsKey

getPrepaidOfferHoldTotal :: (CacheFlow m r, MonadFlow m) => Text -> m HighPrecMoney
getPrepaidOfferHoldTotal = getOfferHoldTotalAtKey . makePrepaidOfferHoldsKey

getPrepaidOfferHoldAmount :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m HighPrecMoney
getPrepaidOfferHoldAmount = getOfferHoldAmountAtKey . makePrepaidOfferHoldsKey

-- | Everything currently held against the wallet: PENDING ledger holds plus
--   live Redis offer holds.
getTotalWalletHoldBalance :: (BeamFlow m r, CacheFlow m r, MonadFlow m) => CounterpartyType -> Text -> m HighPrecMoney
getTotalWalletHoldBalance counterpartyType ownerId = do
  dbHoldBalance <- getWalletHoldBalanceByOwner counterpartyType ownerId
  offerHoldBalance <- getWalletOfferHoldTotal ownerId
  pure (dbHoldBalance + offerHoldBalance)

-- | Statutory deductions for an offer, computed from the base fare: the gross is
--   base + govt + toll + parking, buffered per the wallet config.
estimateOfferDeductions :: DTC.DriverWalletConfig -> DTC.TaxConfig -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney
estimateOfferDeductions dwc taxConfig mbBaseFare govtCharges tollCharges parkingCharge =
  let mbGross = (\bf -> bf + fromMaybe 0 govtCharges + fromMaybe 0 tollCharges + fromMaybe 0 parkingCharge) <$> mbBaseFare
   in estimateBufferedStatutoryDeductions dwc taxConfig mbGross govtCharges tollCharges parkingCharge

-- | Place the provisional wallet/prepaid holds for one driver's offer on a search try.
addOfferHoldsForSearchTry ::
  (CacheFlow m r, MonadFlow m) =>
  DTC.TransporterConfig ->
  Bool -> -- prepaid subscription & wallet enabled for the merchant
  Text -> -- hold owner: fleet owner when present, else driver
  Text -> -- searchTryId
  Maybe DMPM.PaymentInstrument ->
  HighPrecMoney -> -- base fare
  Maybe HighPrecMoney -> -- govt charges
  Maybe HighPrecMoney -> -- toll charges
  Maybe HighPrecMoney -> -- parking charge
  UTCTime -> -- offer validTill
  m ()
addOfferHoldsForSearchTry transporterConfig isPrepaidEnabled holdOwnerId searchTryId paymentInstrument baseFare govtCharges tollCharges parkingCharge validTill = do
  when (cashWalletCheckEnabled transporterConfig.driverWalletConfig && shouldCheckCashWallet paymentInstrument) $ do
    let offerDeduction = estimateOfferDeductions transporterConfig.driverWalletConfig transporterConfig.taxConfig (Just baseFare) govtCharges tollCharges parkingCharge
    when (offerDeduction > 0) $ addWalletOfferHold holdOwnerId searchTryId offerDeduction validTill
  when isPrepaidEnabled $ do
    let prepaidOfferHold = applyFareRecomputeBuffer transporterConfig.driverWalletConfig baseFare
    when (prepaidOfferHold > 0) $ addPrepaidOfferHold holdOwnerId searchTryId prepaidOfferHold validTill

-- | Release both the wallet and prepaid offer holds for a search try.
removeOfferHolds :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m ()
removeOfferHolds ownerId searchTryId = do
  removeWalletOfferHold ownerId searchTryId
  removePrepaidOfferHold ownerId searchTryId

-- | Total live wallet offer holds, excluding the given search try's own hold
--   (used when that hold is about to convert into a real ledger hold).
getWalletOfferHoldTotalExcluding :: (CacheFlow m r, MonadFlow m) => Text -> Maybe Text -> m HighPrecMoney
getWalletOfferHoldTotalExcluding ownerId mbSearchTryId = do
  total <- getWalletOfferHoldTotal ownerId
  own <- maybe (pure 0) (getWalletOfferHoldAmount ownerId) mbSearchTryId
  pure (total - own)

getPrepaidOfferHoldTotalExcluding :: (CacheFlow m r, MonadFlow m) => Text -> Maybe Text -> m HighPrecMoney
getPrepaidOfferHoldTotalExcluding ownerId mbSearchTryId = do
  total <- getPrepaidOfferHoldTotal ownerId
  own <- maybe (pure 0) (getPrepaidOfferHoldAmount ownerId) mbSearchTryId
  pure (total - own)

-- | For a cash ride at assignment time: re-check the wallet can cover the buffered
--   statutory deductions (net of other outstanding offer holds), create the
--   authoritative PENDING ledger hold, and release this search try's offer hold.
reserveWalletForCashRide ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m, Lib.Finance.HasActorInfo m r) =>
  DTC.TransporterConfig ->
  DP.Person ->
  SRB.Booking ->
  Maybe Text -> -- fleet owner id, when the wallet is the fleet's
  Maybe Text -> -- searchTryId whose offer hold converts into this booking hold
  m ()
reserveWalletForCashRide transporterConfig driver booking mbFleetOwnerId mbSearchTryId = do
  isOnline <- resolveIsOnlineFromBooking booking
  unless isOnline $
    when (cashWalletCheckEnabled transporterConfig.driverWalletConfig) $ do
      let (walletCounterpartyType, walletOwnerId) = case mbFleetOwnerId of
            Just fleetOwnerId -> (FLEET_OWNER, fleetOwnerId)
            Nothing -> (DRIVER, driver.id.getId)
          holdAmount =
            estimateBufferedStatutoryDeductions
              transporterConfig.driverWalletConfig
              transporterConfig.taxConfig
              (Just booking.estimatedFare)
              booking.fareParams.govtCharges
              booking.fareParams.tollCharges
              booking.fareParams.parkingCharge
      when (holdAmount > 0) $
        Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey walletOwnerId) 10 10 $ do
          availableBalance <- fromMaybe 0 <$> getWalletAvailableBalanceByOwner walletCounterpartyType walletOwnerId
          otherOfferHolds <- getWalletOfferHoldTotalExcluding walletOwnerId mbSearchTryId
          existingBookingHold <- getPendingWalletHoldAmountByReference walletCounterpartyType walletOwnerId booking.id.getId
          when (availableBalance + existingBookingHold - otherOfferHolds < holdAmount) $ throwError (InvalidRequest "Insufficient earnings balance to cover cash ride deductions.")
          _ <-
            createWalletHold walletCounterpartyType walletOwnerId holdAmount booking.currency booking.providerId.getId booking.merchantOperatingCityId.getId booking.id.getId (Just driver.id.getId) Nothing
              >>= fromEitherM (\err -> InternalError ("Failed to create wallet hold: " <> show err))
          whenJust mbSearchTryId $ removeWalletOfferHold walletOwnerId

getWalletAvailableBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletAvailableBalanceByOwner counterpartyType ownerId = do
  mbBalance <- getWalletBalanceByOwner counterpartyType ownerId
  pendingHold <- getWalletHoldBalanceByOwner counterpartyType ownerId
  pure $ (\balance -> balance - pendingHold) <$> mbBalance

-- | Split a total GST amount into CGST/SGST/IGST proportionally based on GstBreakup percentages.
--   If the total percentage is 0, returns Nothing.
computeGstBreakdown :: DTC.GstBreakup -> HighPrecMoney -> Maybe GstAmountBreakdown
computeGstBreakdown gstBreakup totalGst
  | totalGst <= 0 = Nothing
  | totalPct <= 0 = Nothing
  | otherwise =
    Just
      GstAmountBreakdown
        { cgstAmount = if cgstPct > 0 then Just (totalGst * cgstPct / totalPct) else Nothing,
          sgstAmount = if sgstPct > 0 then Just (totalGst * sgstPct / totalPct) else Nothing,
          igstAmount = if igstPct > 0 then Just (totalGst * igstPct / totalPct) else Nothing
        }
  where
    cgstPct = fromMaybe 0 gstBreakup.cgstPercentage
    sgstPct = fromMaybe 0 gstBreakup.sgstPercentage
    igstPct = fromMaybe 0 gstBreakup.igstPercentage
    totalPct = cgstPct + sgstPct + igstPct

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

estimateWalletDeductions ::
  Maybe Double -> -- effective TDS rate
  HighPrecMoney -> -- baseFare (rideFare at allocation time, which is already baseFare)
  HighPrecMoney -- estimated TDS deduction
estimateWalletDeductions mbTdsRate baseFare =
  case mbTdsRate of
    Just rate | rate > 0 -> max 0 baseFare * realToFrac rate
    _ -> 0

computeGstBreakdownByPlace ::
  DTC.GstBreakup ->
  Maybe Text -> -- supplier state
  Maybe Text -> -- receiver state
  Maybe Text -> -- supplier city
  Maybe Text -> -- receiver city
  HighPrecMoney ->
  Maybe GstAmountBreakdown
computeGstBreakdownByPlace gstBreakup supplierState receiverState supplierCity receiverCity totalGst
  | totalGst <= 0 = Nothing
  | otherwise =
    case comparePlace supplierState receiverState supplierCity receiverCity of
      Just IntraState -> computeGstBreakdown intraStateGstBreakup totalGst
      Just InterState -> computeGstBreakdown interStateGstBreakup totalGst
      Nothing -> computeGstBreakdown gstBreakup totalGst
  where
    comparePlace s1 s2 c1 c2 =
      case (normalizeGeoComponent s1, normalizeGeoComponent s2) of
        (Just leftState, Just rightState) ->
          Just $
            if leftState == rightState
              then IntraState
              else InterState
        _ ->
          case (normalizeGeoComponent c1, normalizeGeoComponent c2) of
            (Just leftCity, Just rightCity) ->
              Just $
                if leftCity == rightCity
                  then IntraState
                  else InterState
            _ -> Nothing

    intraStateGstBreakup =
      DTC.GstBreakup
        { cgstPercentage = gstBreakup.cgstPercentage,
          sgstPercentage = gstBreakup.sgstPercentage,
          igstPercentage = Nothing
        }

    interStateGstBreakup =
      DTC.GstBreakup
        { cgstPercentage = Nothing,
          sgstPercentage = Nothing,
          igstPercentage =
            gstBreakup.igstPercentage
              <|> ((+) <$> gstBreakup.cgstPercentage <*> gstBreakup.sgstPercentage)
        }

-- | Determine GST jurisdiction by comparing the first 2 characters (state code)
--   of the seller and buyer GSTINs, then split the total GST accordingly.
--   GSTIN format: <2-digit state code><10-char PAN><entity><Z><checksum>.
--   Falls back to the supplied 'gstBreakup' as-is when either GSTIN is missing
--   or too short to extract a state code.
computeGstBreakdownGSTIN ::
  DTC.GstBreakup ->
  Maybe Text -> -- seller (supplier) GSTIN
  Maybe Text -> -- buyer (receiver) GSTIN
  HighPrecMoney ->
  Maybe GstAmountBreakdown
computeGstBreakdownGSTIN gstBreakup sellerGstin buyerGstin totalGst
  | totalGst <= 0 = Nothing
  | otherwise =
    case compareStateCode sellerGstin buyerGstin of
      Just IntraState -> computeGstBreakdown intraStateGstBreakup totalGst
      Just InterState -> computeGstBreakdown interStateGstBreakup totalGst
      Nothing -> computeGstBreakdown gstBreakup totalGst
  where
    -- Normalise a GSTIN: trim, upper-case, drop if shorter than 2 chars.
    normaliseGstin mbGstin = do
      gstin <- T.toUpper . T.strip <$> mbGstin
      if T.length gstin >= 2 then Just gstin else Nothing

    -- Compare ONLY the first 2 characters (state code) of seller and buyer GSTIN.
    compareStateCode mbSeller mbBuyer =
      case (normaliseGstin mbSeller, normaliseGstin mbBuyer) of
        (Just seller, Just buyer) ->
          Just $
            if T.take 2 seller == T.take 2 buyer
              then IntraState
              else InterState
        _ -> Nothing

    intraStateGstBreakup =
      DTC.GstBreakup
        { cgstPercentage = gstBreakup.cgstPercentage,
          sgstPercentage = gstBreakup.sgstPercentage,
          igstPercentage = Nothing
        }

    interStateGstBreakup =
      DTC.GstBreakup
        { cgstPercentage = Nothing,
          sgstPercentage = Nothing,
          igstPercentage =
            gstBreakup.igstPercentage
              <|> ((+) <$> gstBreakup.cgstPercentage <*> gstBreakup.sgstPercentage)
        }

data GstJurisdiction = IntraState | InterState

normalizeGeoComponent :: Maybe Text -> Maybe Text
normalizeGeoComponent mbText =
  case T.toLower . T.strip <$> mbText of
    Just value | not (T.null value) -> Just value
    _ -> Nothing
