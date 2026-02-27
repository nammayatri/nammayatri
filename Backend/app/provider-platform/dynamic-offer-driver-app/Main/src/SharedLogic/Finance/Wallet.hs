{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Wallet
  ( walletReferenceBaseRide,
    walletReferenceGSTOnline,
    walletReferenceTollCharges,
    walletReferenceParkingCharges,
    walletReferenceTDSDeductionOnline,
    walletReferenceGSTCash,
    walletReferenceTDSDeductionCash,
    walletReferenceTopup,
    walletReferencePayout,
    walletReferenceDriverCancellationCharges,
    walletReferenceCustomerCancellationCharges,
    walletReferenceCustomerCancellationGST,
    getWalletAccountByOwner,
    getWalletBalanceByOwner,
    getOrCreateWalletAccount,
    createWalletEntryDelta,
    reverseWalletEntryByReference,
    getOrCreateBuyerAssetAccount,
    getOrCreateBuyerExternalAccount,
    getOrCreateDriverLiabilityAccount,
    getOrCreateFleetOwnerLiabilityAccount,
    getOrCreateDriverExpenseAccount,
    getOrCreateFleetOwnerExpenseAccount,
    getOrCreateGovtIndirectLiabilityAccount,
    getOrCreateGovtDirectLiabilityAccount,
    createLedgerTransfer,
    createLedgerTransferAllowZero,
    utcToLocalDay,
    payoutCutoffTimeUTC,
    todayRangeUTC,
    getNonRedeemableBalance,
    LedgerChargeComponent (..),
    LedgerDestination (..),
    createOnlineLedgerEntries,
    WalletInvoiceParams (..),
    createWalletInvoice,
    computeGstBreakdown,
  )
where

import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Time as Time
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import qualified Lib.Finance.Domain.Types.Invoice as Invoice
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.FleetOwnerInformation as QFOI
import Tools.Error

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

-- | Calculate non-redeemable balance: sum of recent ride earnings after payout cutoff.
getNonRedeemableBalance ::
  (BeamFlow m r) =>
  Id Account ->
  NominalDiffTime -> -- timezone offset
  Int -> -- payoutCutOffDays
  UTCTime -> -- current time
  m HighPrecMoney
getNonRedeemableBalance accountId timeDiff cutOffDays now = do
  let cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  entries <- findByAccountWithFilters accountId (Just cutoff) (Just now) Nothing Nothing Nothing (Just [walletReferenceBaseRide])
  pure $ sum $ map (.amount) entries

-- Account helpers

getWalletAccountByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  m (Maybe Account)
getWalletAccountByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure $ find (\acc -> acc.accountType == Liability) accounts

getWalletBalanceByOwner ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

getOrCreateWalletAccount ::
  (BeamFlow m r) =>
  CounterpartyType ->
  Text -> -- Owner ID
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateWalletAccount counterpartyType ownerId currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            counterpartyType = Just counterpartyType,
            counterpartyId = Just ownerId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreatePlatformAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text ->
  Text ->
  m (Either FinanceError Account)
getOrCreatePlatformAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            counterpartyType = Just SELLER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

-- New account helpers for the refactored wallet flow

getOrCreateBuyerAssetAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateBuyerAssetAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Asset,
            counterpartyType = Just BUYER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateBuyerExternalAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateBuyerExternalAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = External,
            counterpartyType = Just BUYER,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateDriverLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Driver ID
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateDriverLiabilityAccount currency driverId =
  getOrCreateWalletAccount DRIVER driverId currency

getOrCreateFleetOwnerLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Fleet owner ID
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateFleetOwnerLiabilityAccount currency fleetOwnerId =
  getOrCreateWalletAccount FLEET_OWNER fleetOwnerId currency

getOrCreateDriverExpenseAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Driver ID
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateDriverExpenseAccount currency driverId merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Expense,
            counterpartyType = Just DRIVER,
            counterpartyId = Just driverId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateFleetOwnerExpenseAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Fleet owner ID
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateFleetOwnerExpenseAccount currency fleetOwnerId merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Expense,
            counterpartyType = Just FLEET_OWNER,
            counterpartyId = Just fleetOwnerId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateGovtIndirectLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateGovtIndirectLiabilityAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            counterpartyType = Just GOVERNMENT_INDIRECT,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

getOrCreateGovtDirectLiabilityAccount ::
  (BeamFlow m r) =>
  Currency ->
  Text -> -- Merchant ID
  Text -> -- Merchant operating city ID
  m (Either FinanceError Account)
getOrCreateGovtDirectLiabilityAccount currency merchantId merchantOperatingCityId = do
  let input =
        AccountInput
          { accountType = Liability,
            counterpartyType = Just GOVERNMENT_DIRECT,
            counterpartyId = Just merchantId,
            currency = currency,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId
          }
  getOrCreateAccount input

-- Ledger transfer functions

createLedgerTransfer ::
  (BeamFlow m r) =>
  Account ->
  Account ->
  HighPrecMoney ->
  Text ->
  Text ->
  m (Either FinanceError (Maybe (Id LedgerEntry)))
createLedgerTransfer fromAccount toAccount amount referenceType referenceId = do
  if amount <= 0
    then pure $ Right Nothing
    else do
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAccount.id,
                toAccountId = toAccount.id,
                amount = amount,
                currency = fromAccount.currency,
                entryType = Lib.Finance.Domain.Types.LedgerEntry.Expense,
                status = SETTLED,
                referenceType = referenceType,
                referenceId = referenceId,
                metadata = Nothing,
                merchantId = fromAccount.merchantId,
                merchantOperatingCityId = fromAccount.merchantOperatingCityId
              }
      entryRes <- createEntryWithBalanceUpdate entryInput
      case entryRes of
        Left err -> pure $ Left err
        Right entry -> pure $ Right (Just entry.id)

-- | Like createLedgerTransfer but allows zero-amount entries (for placeholder entries like TDS)
createLedgerTransferAllowZero ::
  (BeamFlow m r) =>
  Account ->
  Account ->
  HighPrecMoney ->
  Text ->
  Text ->
  m (Either FinanceError (Maybe (Id LedgerEntry)))
createLedgerTransferAllowZero fromAccount toAccount amount referenceType referenceId = do
  if amount < 0
    then pure $ Right Nothing
    else do
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAccount.id,
                toAccountId = toAccount.id,
                amount = amount,
                currency = fromAccount.currency,
                entryType = Lib.Finance.Domain.Types.LedgerEntry.Expense,
                status = SETTLED,
                referenceType = referenceType,
                referenceId = referenceId,
                metadata = Nothing,
                merchantId = fromAccount.merchantId,
                merchantOperatingCityId = fromAccount.merchantOperatingCityId
              }
      entryRes <- createEntryWithBalanceUpdate entryInput
      case entryRes of
        Left err -> pure $ Left err
        Right entry -> pure $ Right (Just entry.id)

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
      mbOwnerAccount <- getOrCreateWalletAccount counterpartyType ownerId currency merchantId merchantOperatingCityId
      mbPlatformAccount <- getOrCreatePlatformAccount currency merchantId merchantOperatingCityId
      case (mbOwnerAccount, mbPlatformAccount) of
        (Right ownerAccount, Right platformAccount) -> do
          let (fromAcc, toAcc, amount, eType) =
                if delta > 0
                  then (platformAccount.id, ownerAccount.id, delta, Lib.Finance.Domain.Types.LedgerEntry.Expense)
                  else (ownerAccount.id, platformAccount.id, abs delta, Lib.Finance.Domain.Types.LedgerEntry.Revenue)
          let entryInput =
                LedgerEntryInput
                  { fromAccountId = fromAcc,
                    toAccountId = toAcc,
                    amount = amount,
                    currency = currency,
                    entryType = eType,
                    status = SETTLED,
                    referenceType = referenceType,
                    referenceId = referenceId,
                    metadata = metadata,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOperatingCityId
                  }
          entryRes <- createEntryWithBalanceUpdate entryInput
          case entryRes of
            Left err -> pure $ Left err
            Right _ -> do
              mbBal <- getBalance ownerAccount.id
              pure $ maybe (Left $ LedgerError AccountMismatch "Balance not found") Right mbBal
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err

reverseWalletEntryByReference ::
  (BeamFlow m r) =>
  Text -> -- Reference type
  Text -> -- Reference ID
  Text -> -- Reason
  m ()
reverseWalletEntryByReference referenceType referenceId reason = do
  entries <- getEntriesByReference referenceType referenceId
  let hasReversal = any (\e -> e.entryType == Reversal) entries
  unless hasReversal $
    case sortOn (Down . (.timestamp)) entries of
      (latest : _) -> do
        _ <- createReversal latest.id reason
        pure ()
      [] -> pure ()

-- Common ledger entry helpers

data LedgerDestination
  = ToDriverOrFleetLiability
  | ToGovtIndirect
  | ToGovtDirect
  deriving (Show, Eq)

data LedgerChargeComponent = LedgerChargeComponent
  { amount :: HighPrecMoney,
    referenceType :: Text,
    destination :: LedgerDestination
  }

-- | Create ledger entries following the online flow pattern:
--   Internally creates all accounts from booking/ride data.
--   For each component: Asset(BUYER) -> External(BUYER), then External(BUYER) -> destination account.
--   Returns the list of entry IDs from the second leg (External -> destination).
createOnlineLedgerEntries ::
  (BeamFlow m r) =>
  SRB.Booking ->
  DRide.Ride ->
  [LedgerChargeComponent] ->
  m [Id LedgerEntry]
createOnlineLedgerEntries booking ride components = do
  let merchantId = fromMaybe booking.providerId ride.merchantId
      mid = merchantId.getId
      mocid = booking.merchantOperatingCityId.getId
      (counterpartyType, driverOrFleetPersonId) =
        case ride.fleetOwnerId of
          Just fleetOwnerId -> (FLEET_OWNER, fleetOwnerId)
          Nothing -> (DRIVER, ride.driverId)
      dofId = driverOrFleetPersonId.getId
  buyerAsset <- getOrCreateBuyerAssetAccount booking.currency mid mocid >>= fromEitherM (\err -> InternalError ("Buyer asset account not found: " <> show err))
  buyerExternal <- getOrCreateBuyerExternalAccount booking.currency mid mocid >>= fromEitherM (\err -> InternalError ("Buyer external account not found: " <> show err))
  driverOrFleetLiability <-
    ( case counterpartyType of
        FLEET_OWNER -> getOrCreateFleetOwnerLiabilityAccount booking.currency dofId mid mocid
        _ -> getOrCreateDriverLiabilityAccount booking.currency dofId mid mocid
      )
      >>= fromEitherM (\err -> InternalError ("Driver/FleetOwner liability account not found: " <> show err))
  govtIndirect <- getOrCreateGovtIndirectLiabilityAccount booking.currency mid mocid >>= fromEitherM (\err -> InternalError ("Government indirect liability account not found: " <> show err))
  govtDirect <- getOrCreateGovtDirectLiabilityAccount booking.currency mid mocid >>= fromEitherM (\err -> InternalError ("Government direct liability account not found: " <> show err))
  results <- forM components $ \comp -> do
    -- Leg 1: Asset(BUYER) -> External(BUYER)
    leg1 <- createLedgerTransfer buyerAsset buyerExternal comp.amount comp.referenceType booking.id.getId
    case leg1 of
      Left err -> pure $ Left err
      Right _ -> do
        -- Leg 2: External(BUYER) -> destination
        let destAccount = case comp.destination of
              ToDriverOrFleetLiability -> driverOrFleetLiability
              ToGovtIndirect -> govtIndirect
              ToGovtDirect -> govtDirect
        createLedgerTransfer buyerExternal destAccount comp.amount comp.referenceType booking.id.getId
  -- Collect results
  let (errs, oks) = partitionEithers results
  case errs of
    (e : _) -> fromEitherM (\err -> InternalError ("Failed to create online ledger entries: " <> show err)) (Left e)
    [] -> pure $ catMaybes oks

-- Common invoice creation helper

data WalletInvoiceParams = WalletInvoiceParams
  { invoiceType :: Invoice.InvoiceType,
    issuedToType :: Text, -- e.g. "CUSTOMER" or "DRIVER"
    issuedToId :: Text,
    issuedToName :: Maybe Text,
    issuedToAddress :: Maybe Text,
    lineItems :: [InvoiceLineItem],
    gstBreakdown :: Maybe GstAmountBreakdown
  }

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

-- | Create an invoice linked to the given ledger entry IDs.
--   Derives merchantId, merchantOpCityId, counterparty, currency, and supplier details from booking/ride.
createWalletInvoice ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  SRB.Booking ->
  DRide.Ride ->
  Maybe DP.Person ->
  WalletInvoiceParams ->
  [Id LedgerEntry] ->
  m ()
createWalletInvoice booking ride mbDriver params entryIds = do
  let merchantId = fromMaybe booking.providerId ride.merchantId
      mid = merchantId.getId
      mocid = booking.merchantOperatingCityId.getId
  when (not $ null entryIds) $ do
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound mid)
    merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist mocid)
    let issuedByAddress = Just $ show merchantOperatingCity.city <> ", " <> show merchantOperatingCity.state <> ", " <> show merchantOperatingCity.country

    -- Fetch supplier details (fleet owner or driver)
    (supplierName', supplierGSTIN', supplierId') <- case ride.fleetOwnerId of
      Just fleetOwnerId -> do
        mbFleetInfo <- QFOI.findByPrimaryKey (cast fleetOwnerId)
        pure
          ( mbFleetInfo >>= (.fleetName),
            mbFleetInfo >>= (.gstNumberDec),
            Just fleetOwnerId.getId
          )
      Nothing -> do
        case mbDriver of
          Just driver ->
            pure
              ( Just (driver.firstName <> maybe "" (" " <>) driver.lastName),
                Nothing,
                Just ride.driverId.getId
              )
          Nothing -> pure (Nothing, Nothing, Just ride.driverId.getId)

    let invoiceInput =
          InvoiceInput
            { invoiceType = params.invoiceType,
              paymentOrderId = Nothing,
              issuedToType = params.issuedToType,
              issuedToId = params.issuedToId,
              issuedToName = params.issuedToName,
              issuedToAddress = params.issuedToAddress,
              issuedByType = "BUYER",
              issuedById = mid,
              issuedByName = Just merchant.name,
              issuedByAddress = issuedByAddress,
              supplierName = supplierName',
              supplierAddress = issuedByAddress,
              supplierGSTIN = supplierGSTIN',
              supplierId = supplierId',
              gstinOfParty = Nothing,
              panOfParty = Nothing,
              tanOfDeductee = Nothing,
              lineItems = params.lineItems,
              gstBreakdown = params.gstBreakdown,
              currency = booking.currency,
              dueAt = Nothing,
              merchantId = mid,
              merchantOperatingCityId = mocid,
              merchantShortId = merchant.shortId.getShortId
            }
    _ <- createInvoice invoiceInput entryIds
    pure ()
