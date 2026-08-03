{-
  Finance.FinanceM

  A monad transformer that encapsulates the financial context
  (merchant, city, currency, counterparty) via ReaderT,
  automatic error lifting via ExceptT, and auto-collection
  of ledger entry IDs via StateT.

  Usage:
    runFinance ctx $ do
      transfer OwnerLiability GovtIndirect gstAmount "GSTCash"
      transfer OwnerLiability GovtDirect   tdsAmount "TDSDeductionCash"
      -- entry IDs are collected automatically

    -- or with explicit ID retrieval:
    runFinance ctx $ do
      transfer OwnerLiability GovtIndirect gstAmount "GSTCash"
      ids <- getEntryIds
      ...

  The FinanceM monad eliminates:
  - Manual threading of (merchantId, merchantOpCityId, currency)
  - Repetitive >>= fromEitherM error bridging
  - 10+ near-identical getOrCreate*Account functions
  - Manual catMaybes to collect entry IDs from transfers
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.FinanceM
  ( -- * Context
    FinanceCtx (..),

    -- * Account Roles
    module Lib.Finance.Types.AccountRole,

    -- * Derivation
    module Lib.Finance.Posting,
    module Lib.Finance.Types.RefTypeConfig,
    module Lib.Finance.Types.ChargeValue,

    -- * The Monad
    FinanceM,
    runFinance,

    -- * Combinators
    account,
    transfer,
    transfer_,
    transferPending,
    transferWithTaxAndCommission,
    transferWithTaxAndCommission_,
    transferPendingWithTaxAndCommission,
    TaxTransferResult (..),
    getKernelRecordedTaxRefs,
    transferAllowZero,
    transferWithoutAttribution,
    getEntryIds,
    liftFinance,
    liftFinanceM,
    getCtx,

    -- * Invoice
    InvoiceConfig (..),
    invoice,

    -- * Standalone Tax Entry
    IndirectTaxConfig (..),
    DirectTaxConfig (..),
    recordIndirectTax,
    recordDirectTax,

    -- * TDS Rate Reason
    TdsRateReason (..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify', runStateT)
import qualified Data.HashMap.Strict as HM
import Domain.Types.Invoice (InvoiceType, IssuedToType)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (MonadFlow)
import Lib.Finance.Account.Interface (AccountInput (..))
import Lib.Finance.Account.Service (getOrCreateAccount)
import Lib.Finance.Core.Types (HasActorInfo)
import Lib.Finance.Domain.Types.Account
import Lib.Finance.Domain.Types.DirectTaxTransaction (DirectTaxTransaction, TdsRateReason (..))
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import qualified Lib.Finance.Domain.Types.FinanceRefTypeConfig as DRC
import Lib.Finance.Domain.Types.IndirectTaxTransaction (GstCreditType, IndirectTaxTransaction)
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import Lib.Finance.Domain.Types.Invoice (Invoice)
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.Error.Types (FinanceError (..))
import Lib.Finance.Invoice.Interface (DirectTaxInput (..), GstAmountBreakdown, IndirectTaxInput (..), InvoiceInput (..), InvoiceLineItem)
import Lib.Finance.Invoice.Service (createDirectTaxEntry, createIndirectTaxEntry, createInvoice)
import Lib.Finance.Ledger.Interface (LedgerEntryInput (..))
import Lib.Finance.Ledger.Service (createEntry, createEntryWithBalanceUpdate)
import Lib.Finance.Posting
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.CachedQueries.FinanceRefTypeConfig as CQRefType
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger
import Lib.Finance.Types.AccountRole
import Lib.Finance.Types.ChargeValue
import Lib.Finance.Types.RefTypeConfig

-- | The financial context for a transaction.
--   Carried implicitly via ReaderT — no more manual argument threading.
--   Invoice-related fields are pre-resolved by the caller so the
--   'invoice' combinator can create invoices without additional DB lookups.
data FinanceCtx = FinanceCtx
  { merchantId :: Text,
    merchantOpCityId :: Text,
    currency :: Currency,
    -- | True for online (card / platform wallet) payments, False for cash.
    --   Selects the account pair used by rider-app transfers; not persisted
    --   on ledger entries.
    isOnline :: Bool,
    counterpartyType :: CounterpartyType,
    counterpartyId :: Text,
    concernedIndividualId :: Maybe Text,
    referenceId :: Text,
    -- | Sub-domain entity every entry posted under this context belongs to (e.g. a refund
    --   request); Nothing when there is none.
    entityReferenceId :: Maybe Text,
    entityReferenceType :: Maybe LE.EntityReferenceType,
    -- Invoice fields (pre-resolved by caller)
    merchantName :: Maybe Text,
    merchantShortId :: Maybe Text,
    issuedByAddress :: Maybe Text,
    supplierName :: Maybe Text,
    supplierGSTIN :: Maybe Text,
    supplierVatNumber :: Maybe Text, -- fleet owner's VAT registration number
    supplierAddress :: Maybe Text, -- fleet owner's address (from stripeAddress) for VAT invoices
    merchantGstin :: Maybe Text, -- merchant's own GSTIN, for issued_by_tax_no (GST)
    merchantVatNumber :: Maybe Text, -- merchant's VAT number, for issued_by_tax_no (VAT)
    supplierId :: Maybe Text,
    panOfParty :: Maybe Text,
    panType :: Maybe Text,
    tdsRateReason :: Maybe TdsRateReason,
    -- | Gates the direct-tax move only. Indirect tax and commission derive
    --   unconditionally: they replace legs the domain already posts, one for
    --   one. TDS relocates cohort selection and the s194O threshold out of the
    --   domain, so it waits for this.
    refTypeConfigurability :: Bool,
    -- | Per-driver materialised TDS rate; wins over the cohort lookup so the
    --   two sources cannot disagree.
    tdsRateOverride :: Maybe ChargeValue,
    -- | Lifetime earnings for the s194O threshold gate. The one input that
    --   changes between postings, which is why it lives here and not in the
    --   profile.
    cumulativeEarnings :: Maybe HighPrecMoney,
    emitLedgerEntries :: Bool,
    fromLocationAddress :: Maybe Text,
    issuedToName :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Caller-provided invoice configuration.
--   Everything else (merchant, supplier, currency, entry IDs) comes from FinanceCtx.
data InvoiceConfig = InvoiceConfig
  { invoiceType :: InvoiceType,
    issuedToType :: IssuedToType,
    issuedToId :: Text,
    issuedToName :: Maybe Text,
    issuedToAddress :: Maybe Text,
    referenceId :: Maybe Text,
    lineItems :: [InvoiceLineItem],
    gstBreakdown :: Maybe GstAmountBreakdown,
    -- VAT integration fields
    isVat :: Bool,
    issuedToTaxNo :: Maybe Text,
    issuedByTaxNo :: Maybe Text,
    paymentMode :: Maybe Text,
    -- Period bounds for aggregated invoices; Nothing for per-event invoices.
    periodStart :: Maybe UTCTime,
    periodEnd :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic)

-- | Accumulated state within a FinanceM computation.
--   Entry IDs from all transfers are collected automatically.
data FinanceState = FinanceState
  { collectedEntryIds :: [Id LE.LedgerEntry],
    -- | Resolved once per 'runFinance' block, on first use. 'Nothing' means not
    --   yet loaded — distinct from a resolved-but-empty profile.
    refTypeProfile :: Maybe (HM.HashMap Text DRC.FinanceRefTypeConfig),
    -- | Ref types whose tax transaction this block already wrote, so the
    --   invoice sweep does not write a second one.
    kernelRecordedTaxRefs :: [Text]
  }
  deriving (Eq, Show, Generic)

emptyState :: FinanceState
emptyState = FinanceState {collectedEntryIds = [], refTypeProfile = Nothing, kernelRecordedTaxRefs = []}

-- | The FinanceM monad transformer.
--   ReaderT for context threading, StateT for entry ID collection,
--   ExceptT for error short-circuiting.
newtype FinanceM m a = FinanceM
  { unFinanceM :: ReaderT FinanceCtx (StateT FinanceState (ExceptT FinanceError m)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader FinanceCtx,
      MonadState FinanceState,
      MonadError FinanceError
    )

instance MonadTrans FinanceM where
  lift = FinanceM . lift . lift . lift

-- | Run a FinanceM computation.
--   Returns (Either FinanceError (a, [Id LedgerEntry])).
--   The entry IDs are all entries created by 'transfer'/'transferAllowZero'
--   during the computation, in order.
runFinance ::
  (MonadFlow m) =>
  FinanceCtx ->
  FinanceM m a ->
  m (Either FinanceError (a, [Id LE.LedgerEntry]))
runFinance ctx action = do
  result <- runExceptT (runStateT (runReaderT (unFinanceM action) ctx) emptyState)
  case result of
    Left err -> pure $ Left err
    Right (a, st) -> pure $ Right (a, st.collectedEntryIds)

-- | Get the current financial context.
getCtx :: (Monad m) => FinanceM m FinanceCtx
getCtx = ask

-- | Get the entry IDs collected so far in this FinanceM computation.
getEntryIds :: (Monad m) => FinanceM m [Id LE.LedgerEntry]
getEntryIds = gets (.collectedEntryIds)

-- | Lift an @Either FinanceError a@ value into FinanceM.
liftFinance :: (Monad m) => Either FinanceError a -> FinanceM m a
liftFinance (Left err) = throwError err
liftFinance (Right a) = pure a

-- | Lift an @m (Either FinanceError a)@ action into FinanceM.
liftFinanceM :: (Monad m) => m (Either FinanceError a) -> FinanceM m a
liftFinanceM action = do
  result <- lift action
  liftFinance result

-- | Resolve an AccountRole to a concrete Account using the FinanceCtx.
--   This single function replaces 10+ getOrCreate*Account helpers.
account :: (BeamFlow.BeamFlow m r) => AccountRole -> FinanceM m Account
account role = do
  ctx <- ask
  let input = roleToInput ctx role
  liftFinanceM (getOrCreateAccount input)

-- | Internal: map an AccountRole to an AccountInput using context.
roleToInput :: FinanceCtx -> AccountRole -> AccountInput
roleToInput ctx = \case
  BuyerAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just BUYER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  BuyerExternal ->
    AccountInput
      { accountType = External,
        counterpartyType = Just BUYER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  BuyerExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just BUYER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  BuyerControl ->
    AccountInput
      { accountType = Control,
        counterpartyType = Just BUYER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  OwnerLiability ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just ctx.counterpartyType,
        counterpartyId = Just ctx.counterpartyId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  OwnerControl ->
    AccountInput
      { accountType = Control,
        counterpartyType = Just ctx.counterpartyType,
        counterpartyId = Just ctx.counterpartyId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  OwnerExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just ctx.counterpartyType,
        counterpartyId = Just ctx.counterpartyId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtIndirect ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just GOVERNMENT_INDIRECT,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtDirect ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just GOVERNMENT_DIRECT,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just GOVERNMENT_INDIRECT,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PlatformAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PrepaidOwner ->
    AccountInput
      { accountType = RideCredit,
        counterpartyType = Just ctx.counterpartyType,
        counterpartyId = Just ctx.counterpartyId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerLiability ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerRideCredit ->
    AccountInput
      { accountType = RideCredit,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerRevenue ->
    AccountInput
      { accountType = Revenue,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtDirectAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just GOVERNMENT_DIRECT,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtDirectExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just GOVERNMENT_DIRECT,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  ParkingFeeRecipient ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just AIRPORT,
        counterpartyId = Just ctx.merchantOpCityId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PGPaymentExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just PG_PAYMENT_JUSPAY,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PGPaymentLiability ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just PG_PAYMENT_JUSPAY,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PGPayoutExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just PG_PAYOUT_JUSPAY,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PGPayoutLiability ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just PG_PAYOUT_JUSPAY,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PGGstAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just GOVERNMENT_INDIRECT,
        counterpartyId = Just ctx.merchantId,
        subLedger = Nothing,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }

-- | Internal helper: append an entry ID to the collected list.
collectEntryId :: (Monad m) => Id LE.LedgerEntry -> FinanceM m ()
collectEntryId entryId =
  modify' (\st -> st {collectedEntryIds = st.collectedEntryIds <> [entryId]})

-- | Transfer money between two account roles.
--   Skips if amount <= 0.  Automatically collects the entry ID.
--   Returns the entry ID if created (Nothing if skipped due to amount <= 0).
transfer ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m (Maybe (Id LE.LedgerEntry))
transfer fromRole toRole amount refType = do
  ctx <- ask
  if amount <= 0 || not ctx.emitLedgerEntries
    then pure Nothing
    else do
      fromAcc <- account fromRole
      toAcc <- account toRole
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAcc.id,
                toAccountId = toAcc.id,
                concernedIndividualId = ctx.concernedIndividualId,
                amount = amount,
                currency = ctx.currency,
                entryType = LE.Expense,
                status = LE.SETTLED,
                referenceType = refType,
                referenceId = ctx.referenceId,
                entityReferenceId = ctx.entityReferenceId,
                entityReferenceType = ctx.entityReferenceType,
                metadata = Nothing,
                merchantId = ctx.merchantId,
                merchantOperatingCityId = ctx.merchantOpCityId,
                settlementStatus = Nothing,
                appliedTreatment = Nothing
              }
      result <- liftFinanceM (createEntryWithBalanceUpdate entryInput)
      collectEntryId result.id
      pure (Just result.id)

transferWithoutAttribution ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m (Maybe (Id LE.LedgerEntry))
transferWithoutAttribution fromRole toRole amount refType = do
  ctx <- ask
  if amount <= 0 || not ctx.emitLedgerEntries
    then pure Nothing
    else do
      fromAcc <- account fromRole
      toAcc <- account toRole
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAcc.id,
                toAccountId = toAcc.id,
                concernedIndividualId = Nothing,
                amount = amount,
                currency = ctx.currency,
                entryType = LE.Expense,
                status = LE.SETTLED,
                referenceType = refType,
                referenceId = ctx.referenceId,
                entityReferenceId = ctx.entityReferenceId,
                entityReferenceType = ctx.entityReferenceType,
                metadata = Nothing,
                merchantId = ctx.merchantId,
                merchantOperatingCityId = ctx.merchantOpCityId,
                settlementStatus = Nothing,
                appliedTreatment = Nothing
              }
      result <- liftFinanceM (createEntryWithBalanceUpdate entryInput)
      collectEntryId result.id
      pure (Just result.id)

-- | Like 'transfer' but does NOT collect the entry ID.
--   Use for intermediate legs (e.g. BuyerAsset -> BuyerExternal)
--   that should not appear on invoices.
transfer_ ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m ()
transfer_ fromRole toRole amount refType = do
  ctx <- ask
  when (amount > 0 && ctx.emitLedgerEntries) $ do
    fromAcc <- account fromRole
    toAcc <- account toRole
    let entryInput =
          LedgerEntryInput
            { fromAccountId = fromAcc.id,
              toAccountId = toAcc.id,
              concernedIndividualId = ctx.concernedIndividualId,
              amount = amount,
              currency = ctx.currency,
              entryType = LE.Expense,
              status = LE.SETTLED,
              referenceType = refType,
              referenceId = ctx.referenceId,
              entityReferenceId = ctx.entityReferenceId,
              entityReferenceType = ctx.entityReferenceType,
              metadata = Nothing,
              merchantId = ctx.merchantId,
              merchantOperatingCityId = ctx.merchantOpCityId,
              settlementStatus = Nothing,
              appliedTreatment = Nothing
            }
    _ <- liftFinanceM (createEntryWithBalanceUpdate entryInput)
    pure ()

-- | Like 'transfer' but creates entries with PENDING status and does NOT update
--   account balances.  Use this for entries that will be settled later
--   (e.g. rider payment obligations before payment capture).
--   Automatically collects the entry ID.
transferPending ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m (Maybe (Id LE.LedgerEntry))
transferPending fromRole toRole amount refType = do
  ctx <- ask
  if amount <= 0 || not ctx.emitLedgerEntries
    then pure Nothing
    else do
      fromAcc <- account fromRole
      toAcc <- account toRole
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAcc.id,
                toAccountId = toAcc.id,
                concernedIndividualId = ctx.concernedIndividualId,
                amount = amount,
                currency = ctx.currency,
                entryType = LE.Expense,
                status = LE.PENDING,
                referenceType = refType,
                referenceId = ctx.referenceId,
                entityReferenceId = ctx.entityReferenceId,
                entityReferenceType = ctx.entityReferenceType,
                metadata = Nothing,
                merchantId = ctx.merchantId,
                merchantOperatingCityId = ctx.merchantOpCityId,
                settlementStatus = Nothing,
                appliedTreatment = Nothing
              }
      result <- liftFinanceM (createEntry entryInput)
      collectEntryId result.id
      pure (Just result.id)

-- | Like 'transfer' but allows zero-amount entries (e.g. placeholder TDS entries).
--   Skips only for negative amounts.  Automatically collects the entry ID.
transferAllowZero ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m (Maybe (Id LE.LedgerEntry))
transferAllowZero fromRole toRole amount refType = do
  ctx <- ask
  if amount < 0 || not ctx.emitLedgerEntries
    then pure Nothing
    else do
      fromAcc <- account fromRole
      toAcc <- account toRole
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAcc.id,
                toAccountId = toAcc.id,
                concernedIndividualId = ctx.concernedIndividualId,
                amount = amount,
                currency = ctx.currency,
                entryType = LE.Expense,
                status = LE.SETTLED,
                referenceType = refType,
                referenceId = ctx.referenceId,
                entityReferenceId = ctx.entityReferenceId,
                entityReferenceType = ctx.entityReferenceType,
                metadata = Nothing,
                merchantId = ctx.merchantId,
                merchantOperatingCityId = ctx.merchantOpCityId,
                settlementStatus = Nothing,
                appliedTreatment = Nothing
              }
      result <- liftFinanceM (createEntryWithBalanceUpdate entryInput)
      collectEntryId result.id
      pure (Just result.id)

-- | Create an invoice using the auto-collected entry IDs and pre-resolved
--   context from 'FinanceCtx'.  No-ops if no entries have been created.
--
--   Example:
--   @
--     runFinance ctx $ do
--       transfer OwnerLiability GovtIndirect gstAmount "GSTCash"
--       transfer OwnerLiability GovtDirect   tdsAmount "TDSCash"
--       invoice InvoiceConfig
--         { invoiceType = Ride
--         , issuedToType = "CUSTOMER"
--         , issuedToId = riderId
--         , lineItems = [...]
--         , ...
--         }
--   @
invoice ::
  ( BeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasActorInfo m r
  ) =>
  InvoiceConfig ->
  FinanceM m (Maybe (Id Invoice))
invoice config = do
  ctx <- ask
  if not ctx.emitLedgerEntries
    then pure Nothing
    else invoiceInner ctx config

invoiceInner ::
  ( BeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasActorInfo m r
  ) =>
  FinanceCtx ->
  InvoiceConfig ->
  FinanceM m (Maybe (Id Invoice))
invoiceInner ctx config = do
  ids <- getEntryIds
  let invoiceInput =
        InvoiceInput
          { invoiceType = config.invoiceType,
            entityReferenceId = Nothing,
            referenceInvoiceNumber = Nothing,
            issuedToType = config.issuedToType,
            issuedToId = config.issuedToId,
            issuedToName = config.issuedToName,
            issuedToAddress = config.issuedToAddress,
            issuedByType = "BUYER",
            issuedById = ctx.merchantId,
            issuedByName = ctx.merchantName,
            issuedByAddress = ctx.issuedByAddress,
            supplierName = ctx.supplierName,
            supplierAddress = if config.isVat then ctx.supplierAddress <|> ctx.issuedByAddress else ctx.issuedByAddress,
            supplierGSTIN = ctx.supplierGSTIN,
            supplierTaxNo = if config.isVat then ctx.supplierVatNumber else ctx.supplierGSTIN,
            supplierId = ctx.supplierId,
            merchantGstin = ctx.merchantGstin,
            referenceId = config.referenceId,
            gstinOfParty = Nothing,
            panOfParty = ctx.panOfParty,
            panType = ctx.panType,
            counterpartyId = ctx.counterpartyId,
            tdsRateReason = ctx.tdsRateReason,
            tanOfDeductee = Nothing,
            lineItems = config.lineItems,
            gstBreakdown = config.gstBreakdown,
            currency = ctx.currency,
            dueAt = Nothing,
            periodStart = config.periodStart,
            periodEnd = config.periodEnd,
            merchantId = ctx.merchantId,
            merchantOperatingCityId = ctx.merchantOpCityId,
            merchantShortId = fromMaybe ctx.merchantId ctx.merchantShortId,
            -- VAT integration fields
            isVat = config.isVat,
            issuedToTaxNo = config.issuedToTaxNo,
            issuedByTaxNo = if config.isVat then ctx.merchantVatNumber else ctx.merchantGstin,
            paymentMode = config.paymentMode
          }
  inv <- liftFinanceM (createInvoice invoiceInput ids)
  pure (Just inv.id)

-- | Caller-provided config for recording a standalone indirect tax (GST/VAT) entry.
--   merchantId and merchantOperatingCityId come from FinanceCtx.
data IndirectTaxConfig = IndirectTaxConfig
  { transactionType :: IndirectTax.TransactionType,
    referenceId :: Text,
    taxableValue :: HighPrecMoney,
    totalTaxAmount :: HighPrecMoney,
    gstBreakdown :: Maybe GstAmountBreakdown,
    taxCreditType :: GstCreditType,
    counterpartyId :: Text,
    gstinOfParty :: Maybe Text,
    sacCode :: Maybe Text,
    externalCharges :: Maybe HighPrecMoney,
    isVat :: Bool,
    issuedToTaxNo :: Maybe Text,
    issuedByTaxNo :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Caller-provided config for recording a standalone direct tax (TDS) entry.
--   merchantId and merchantOperatingCityId come from FinanceCtx.
data DirectTaxConfig = DirectTaxConfig
  { transactionType :: DirectTax.TransactionType,
    referenceId :: Text,
    grossAmount :: HighPrecMoney,
    tdsAmount :: HighPrecMoney,
    tdsTreatment :: DirectTax.TdsTreatment,
    counterpartyId :: Text,
    panOfParty :: Maybe Text,
    panType :: Maybe Text,
    tdsRateReason :: Maybe TdsRateReason,
    tanOfDeductee :: Maybe Text,
    tdsSection :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Record a standalone indirect tax (GST) transaction without creating an invoice.
--   Uses FinanceCtx for merchantId and merchantOperatingCityId.
--
--   Example:
--   @
--     runFinance ctx $ do
--       transfer_ PGPaymentLiability PGGstAsset gstAmount "PGFeeGST"
--       recordIndirectTax IndirectTaxConfig
--         { transactionType = PGFee
--         , referenceId = orderId
--         , taxableValue = pgBaseFee
--         , totalGstAmount = gstAmount
--         , gstBreakdown = Nothing
--         , gstCreditType = Input
--         , counterpartyId = pgProviderId
--         , ...
--         }
--   @
recordIndirectTax ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  IndirectTaxConfig ->
  FinanceM m (Id IndirectTaxTransaction)
recordIndirectTax config = do
  ctx <- ask
  let input =
        IndirectTaxInput
          { transactionType = config.transactionType,
            referenceId = config.referenceId,
            taxableValue = config.taxableValue,
            totalTaxAmount = config.totalTaxAmount,
            gstBreakdown = config.gstBreakdown,
            taxCreditType = config.taxCreditType,
            counterpartyId = config.counterpartyId,
            gstinOfParty = config.gstinOfParty,
            sacCode = config.sacCode,
            externalCharges = config.externalCharges,
            invoiceNumber = Nothing,
            merchantId = ctx.merchantId,
            merchantOperatingCityId = ctx.merchantOpCityId,
            isVat = config.isVat,
            issuedToTaxNo = config.issuedToTaxNo,
            issuedByTaxNo = config.issuedByTaxNo
          }
  txn <- lift (createIndirectTaxEntry input)
  pure txn.id

-- | Record a standalone direct tax (TDS) transaction without creating an invoice.
--   Uses FinanceCtx for merchantId and merchantOperatingCityId.
recordDirectTax ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  DirectTaxConfig ->
  FinanceM m (Id DirectTaxTransaction)
recordDirectTax config = do
  ctx <- ask
  let input =
        DirectTaxInput
          { transactionType = config.transactionType,
            referenceId = config.referenceId,
            grossAmount = config.grossAmount,
            tdsAmount = config.tdsAmount,
            tdsTreatment = config.tdsTreatment,
            counterpartyId = config.counterpartyId,
            panOfParty = config.panOfParty,
            panType = config.panType,
            tdsRateReason = config.tdsRateReason,
            tanOfDeductee = config.tanOfDeductee,
            tdsSection = config.tdsSection,
            invoiceNumber = Nothing,
            merchantId = ctx.merchantId,
            merchantOperatingCityId = ctx.merchantOpCityId
          }
  txn <- lift (createDirectTaxEntry input)
  pure txn.id

-- | What 'transferWithTaxAndCommission' posted, so callers that must mirror it
--   — the commission reversal at EndRide, for one — can read the amounts off
--   the result instead of recomputing them.
data TaxTransferResult = TaxTransferResult
  { netEntryId :: Maybe (Id LE.LedgerEntry),
    netAmount :: HighPrecMoney,
    indirectTaxEntryIds :: [Id LE.LedgerEntry],
    indirectTaxAmount :: HighPrecMoney,
    directTaxEntryId :: Maybe (Id LE.LedgerEntry),
    directTaxAmount :: HighPrecMoney,
    commissionEntryIds :: [Id LE.LedgerEntry],
    commissionAmount :: HighPrecMoney
  }
  deriving (Eq, Show, Generic)

getKernelRecordedTaxRefs :: (Monad m) => FinanceM m [Text]
getKernelRecordedTaxRefs = gets (.kernelRecordedTaxRefs)

-- | Resolve the treatment governing this transaction, once per 'runFinance'
--   block. Prefers what the transaction already posted over the current
--   catalogue, so a refund expands the way its charge did.
resolveProfileMemo :: (BeamFlow.BeamFlow m r) => FinanceM m (HM.HashMap Text DRC.FinanceRefTypeConfig)
resolveProfileMemo =
  gets (.refTypeProfile) >>= \case
    Just p -> pure p
    Nothing -> do
      ctx <- ask
      prior <- lift (QLedger.findAllByReferenceId ctx.referenceId)
      let fromPrior = CQRefType.profileFromEntries prior
      p <-
        if HM.null fromPrior
          then lift (CQRefType.profileFromCatalogue ctx.merchantOpCityId)
          else pure fromPrior
      modify' (\st -> st {refTypeProfile = Just p})
      pure p

-- | Post a charge and everything the catalogue says follows from it: the
--   indirect-tax leg, the TDS leg, and the commission leg with its own tax.
--
--   The amount is gross unless the ref type is configured exclusive. Account
--   roles stay the caller's: derived legs are rewrites of the pair, never a
--   pair invented here.
transferWithTaxAndCommission ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  DerivedRefs ->
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text ->
  FinanceM m TaxTransferResult
transferWithTaxAndCommission = postDerived LE.SETTLED True

-- | As 'transferWithTaxAndCommission', but nothing is collected onto invoices —
--   the derived counterpart of 'transfer_'.
transferWithTaxAndCommission_ ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  DerivedRefs ->
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text ->
  FinanceM m TaxTransferResult
transferWithTaxAndCommission_ = postDerived LE.SETTLED False

transferPendingWithTaxAndCommission ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  DerivedRefs ->
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text ->
  FinanceM m TaxTransferResult
transferPendingWithTaxAndCommission = postDerived LE.PENDING True

postDerived ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  LE.EntryStatus ->
  Bool ->
  DerivedRefs ->
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text ->
  FinanceM m TaxTransferResult
postDerived status collectLegs refs fromRole toRole amount refType = do
  ctx <- ask
  if amount <= 0 || not ctx.emitLedgerEntries
    then pure (emptyResult amount)
    else do
      profile <- resolveProfileMemo
      let env =
            Env
              { envMode = if ctx.isOnline then Online else Cash,
                envTdsRateReason = ctx.tdsRateReason,
                envTdsRateOverride = ctx.tdsRateOverride,
                envCumulativeEarnings = ctx.cumulativeEarnings
              }
          posting = Posting {refType = refType, payer = fromRole, payee = toRole, amount = amount}
          -- With the flag off the domain still computes and posts TDS itself,
          -- so dropping the ref here is what stops us posting it twice.
          refs' = if ctx.refTypeConfigurability then refs else refs {directTaxRef = Nothing}
          expanded = expandPosting profile env refs' posting
          legs = if collectLegs then expanded else map (\l -> l {collect = False}) expanded
          treatment = HM.lookup refType profile
      posted <- traverse (emitLeg status treatment) legs
      let pairs = zip legs posted
          idsOf k = [i | (l, Just i) <- pairs, l.isDerivedTax == k]
          sumOf k = sum [l.amount | l <- legs, l.isDerivedTax == k]
      forM_ [l | l <- legs, isJust l.isDerivedTax] $ \l ->
        modify' (\st -> st {kernelRecordedTaxRefs = l.refType : st.kernelRecordedTaxRefs})
      pure
        TaxTransferResult
          { netEntryId = listToMaybe (idsOf Nothing),
            netAmount = sumOf Nothing,
            indirectTaxEntryIds = idsOf (Just IndirectTax),
            indirectTaxAmount = sumOf (Just IndirectTax),
            directTaxEntryId = listToMaybe (idsOf (Just DirectTax)),
            directTaxAmount = sumOf (Just DirectTax),
            commissionEntryIds = [],
            commissionAmount = 0
          }
  where
    emptyResult amt =
      TaxTransferResult
        { netEntryId = Nothing,
          netAmount = amt,
          indirectTaxEntryIds = [],
          indirectTaxAmount = 0,
          directTaxEntryId = Nothing,
          directTaxAmount = 0,
          commissionEntryIds = [],
          commissionAmount = 0
        }

-- | Write one expanded leg through the same single-leg path the primitives use,
--   stamping the treatment that produced it.
emitLeg ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  LE.EntryStatus ->
  Maybe DRC.FinanceRefTypeConfig ->
  Leg ->
  FinanceM m (Maybe (Id LE.LedgerEntry))
emitLeg status treatment leg
  | leg.amount <= 0 = pure Nothing
  | otherwise = do
    ctx <- ask
    fromAcc <- account leg.from
    toAcc <- account leg.to
    let entryInput =
          LedgerEntryInput
            { fromAccountId = fromAcc.id,
              toAccountId = toAcc.id,
              concernedIndividualId = ctx.concernedIndividualId,
              amount = leg.amount,
              currency = ctx.currency,
              entryType = LE.Expense,
              status = status,
              referenceType = leg.refType,
              referenceId = ctx.referenceId,
              entityReferenceId = ctx.entityReferenceId,
              entityReferenceType = ctx.entityReferenceType,
              metadata = Nothing,
              merchantId = ctx.merchantId,
              merchantOperatingCityId = ctx.merchantOpCityId,
              settlementStatus = Nothing,
              appliedTreatment = if isJust leg.isDerivedTax then treatment else Nothing
            }
    result <-
      liftFinanceM $
        if status == LE.PENDING then createEntry entryInput else createEntryWithBalanceUpdate entryInput
    when leg.collect $ collectEntryId result.id
    pure (Just result.id)
