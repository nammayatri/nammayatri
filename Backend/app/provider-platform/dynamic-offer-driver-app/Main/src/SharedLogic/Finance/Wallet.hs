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
    financeCtxFromRide,
    buildFinanceCtx,
    resolveIsOnlineFromBooking,
    walletReferenceCommissionOnline,
    walletReferenceCommissionCash,
    walletReferenceVATOnline,
    walletReferenceVATCash,
    walletReferenceD2DReferral,
    walletReferenceAirportCashRecharge,
    walletReferenceAirportEntryFeeGST,
    walletReferenceAirportEntryFee,
    walletReferenceVATInput,
    walletReferenceCancellationVATInput,
    walletReferenceTips,
    walletReferenceDiscountsOnline,
    walletReferenceDiscountsCash,
    walletReferenceVATAbsorbedOnDiscount,
    getRedeemableEntryIds,
    settleWalletEntries,
    getPayoutEligibilityData,
    computeTdsRateReason,
    computeEffectiveTdsRate,
    estimateWalletDeductions,
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
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import qualified Lib.Finance.Domain.Types.LedgerEntry
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

walletReferenceCommissionOnline :: Text
walletReferenceCommissionOnline = "CommissionOnline"

walletReferenceCommissionCash :: Text
walletReferenceCommissionCash = "CommissionCash"

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

-- | VAT share inside a customer-visible discount that BAP absorbs on the
--   driver's behalf. Together with the matching 'walletReferenceDiscounts*'
--   ref (Online or Cash) the sum equals
--   the total discount shown to the rider.
walletReferenceVATAbsorbedOnDiscount :: Text
walletReferenceVATAbsorbedOnDiscount = "VATAbsorbedOnDiscount"

walletReferenceD2DReferral :: Text
walletReferenceD2DReferral = "D2DReferral"

-- | Reference type for airport booth cash recharge (idempotent by referenceId; booth operator took amount)
walletReferenceAirportCashRecharge :: Text
walletReferenceAirportCashRecharge = "AirportCashRecharge"

-- | Reference type for airport entry fee GST ledger entry at EndRide (third party GST)
walletReferenceAirportEntryFeeGST :: Text
walletReferenceAirportEntryFeeGST = "AirportEntryFeeGST"

-- | Reference type for airport entry fee (airport portion) ledger entry at EndRide (third party charges)
walletReferenceAirportEntryFee :: Text
walletReferenceAirportEntryFee = "AirportEntryFee"

walletReferenceWalletIncentive :: Text
walletReferenceWalletIncentive = "WalletIncentive"

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
    walletReferenceTopup,
    walletReferenceAirportCashRecharge,
    walletReferenceD2DReferral,
    walletReferenceCustomerCancellationCharges,
    walletReferenceDriverCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceCommissionOnline,
    walletReferenceCommissionCash,
    walletReferenceWalletIncentive,
    walletReferenceVATOnline,
    walletReferenceVATCash,
    walletReferenceVATInput,
    walletReferenceCancellationVATInput,
    walletReferenceTips,
    walletReferenceDiscountsOnline,
    walletReferenceDiscountsCash,
    walletReferenceVATAbsorbedOnDiscount
  ]

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
getWalletAccountByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure $ find (\acc -> acc.accountType == Liability) accounts

-- | Returns the driver's Control (cash-earnings memo) account, if any. Distinct
--   from the Liability wallet account — Control tracks cumulative cash ride
--   earnings (direct rider → driver), while Liability tracks what the platform
--   actually owes the driver.
getControlAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getControlAccountByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure $ find (\acc -> acc.accountType == Control) accounts

-- | Fetch both Liability (real wallet) and Control (cash-earnings memo)
--   accounts for an owner. Used by the driver wallet transactions feed which
--   merges entries across both so cash rides surface alongside online earnings.
getWalletAndControlAccountsByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe Account, Maybe Account)
getWalletAndControlAccountsByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure
    ( find (\acc -> acc.accountType == Liability) accounts,
      find (\acc -> acc.accountType == Control) accounts
    )

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
  let configDefaultTdsRate = transporterConfig.taxConfig.defaultTdsRate
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
        fromLocationAddress = Nothing,
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
        fromLocationAddress = Nothing,
        issuedToName = Nothing
      }

-- Wallet entry delta (for topup/payout)

createWalletEntryDelta ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  HighPrecMoney -> -- Delta (positive credit, negative debit)
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  Text -> -- Reference type
  Text -> -- Reference ID
  Maybe Value ->
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
                currency = currency,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOperatingCityId
              }
          platformInput =
            AccountInput
              { accountType = Asset,
                counterpartyType = Just SELLER,
                counterpartyId = Just merchantId,
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
--   This avoids fetching all entries into Haskell memory.
getPayoutEligibilityData ::
  (BeamFlow m r) =>
  Id Account ->
  UTCTime -> -- payout cutoff time
  UTCTime -> -- current time (upper bound)
  m (HighPrecMoney, [Id LedgerEntry])
getPayoutEligibilityData accountId cutoff now = do
  -- Query 1: credits after cutoff (for non-redeemable balance)
  creditsAfterCutoff <- findCreditsByAccountAfterTime accountId cutoff now
  let nonRedeemableBalance = sum $ map (.amount) creditsAfterCutoff
  -- Query 2: unsettled entries before cutoff (for redeemable IDs)
  unsettledBeforeCutoff <- findUnsettledByAccountBeforeTime accountId cutoff
  let redeemableIds = map (.id) unsettledBeforeCutoff
  pure (nonRedeemableBalance, redeemableIds)

-- | Mark a list of wallet ledger entries as paid out.
--   Called by the payout webhook handler after successful disbursement.
settleWalletEntries ::
  (BeamFlow m r) =>
  [Id LedgerEntry] -> -- entry IDs to settle
  Text -> -- PayoutRequest ID
  m ()
settleWalletEntries entryIds payoutRequestId =
  markEntriesAsPaidOut entryIds payoutRequestId

-- | Resolve the effective TDS rate for a driver/fleet owner.
--   If PAN is invalid/missing → invalidPanTdsRate.
--   If PAN is valid → custom rate (from driverInfo/fleetOwnerInfo) or config default.
computeEffectiveTdsRate ::
  Maybe DPanCard.DriverPanCard -> -- PAN card info
  Maybe Double -> -- custom TDS rate (driverInfo.tdsRate or fleetOwnerInfo.tdsRate)
  Maybe Double -> -- config defaultTdsRate
  Double -> -- invalidPanTdsRate
  Maybe Double -- effective rate
computeEffectiveTdsRate mbPanCard mbCustomRate configDefaultTdsRate invalidPanTdsRate_ =
  let hasValidPan = maybe False (\pan -> pan.verificationStatus == Documents.VALID) mbPanCard
      panType = mbPanCard >>= (.docType)
      panTypeEligible = case panType of
        Just DPanCard.BUSINESS -> True
        _ -> True
      isPanValid = hasValidPan && panTypeEligible
   in if isPanValid
        then case mbCustomRate of
          Just _ -> mbCustomRate
          Nothing -> configDefaultTdsRate
        else Just invalidPanTdsRate_

-- | Estimate wallet deductions (TDS only) for a given baseFare.
--   GST (govtCharges) comes from fareParams at ride end, not recalculated here.
--   At allocation time, rideFare = searchTry.baseFare (already excludes GST).
--   tdsRate is a fractional Double (e.g. 0.01 for 1%).
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

data GstJurisdiction = IntraState | InterState

normalizeGeoComponent :: Maybe Text -> Maybe Text
normalizeGeoComponent mbText =
  case T.toLower . T.strip <$> mbText of
    Just value | not (T.null value) -> Just value
    _ -> Nothing
