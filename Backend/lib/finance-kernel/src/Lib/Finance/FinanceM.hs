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
    AccountRole (..),

    -- * The Monad
    FinanceM,
    runFinance,

    -- * Combinators
    account,
    transfer,
    transfer_,
    transferAllowZero,
    getEntryIds,
    liftFinance,
    liftFinanceM,
    getCtx,

    -- * Invoice
    InvoiceConfig (..),
    invoice,
  )
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State.Strict (MonadState, StateT, gets, modify', runStateT)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (MonadFlow)
import Lib.Finance.Account.Interface (AccountInput (..))
import Lib.Finance.Account.Service (getOrCreateAccount)
import Lib.Finance.Domain.Types.Account
import Lib.Finance.Domain.Types.Invoice (InvoiceType)
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.Error.Types (FinanceError (..))
import Lib.Finance.Invoice.Interface (GstAmountBreakdown, InvoiceInput (..), InvoiceLineItem)
import Lib.Finance.Invoice.Service (createInvoice)
import Lib.Finance.Ledger.Interface (LedgerEntryInput (..))
import Lib.Finance.Ledger.Service (createEntryWithBalanceUpdate)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow

-- | The financial context for a transaction.
--   Carried implicitly via ReaderT — no more manual argument threading.
--   Invoice-related fields are pre-resolved by the caller so the
--   'invoice' combinator can create invoices without additional DB lookups.
data FinanceCtx = FinanceCtx
  { merchantId :: Text,
    merchantOpCityId :: Text,
    currency :: Currency,
    counterpartyType :: CounterpartyType,
    counterpartyId :: Text,
    referenceId :: Text,
    -- Invoice fields (pre-resolved by caller)
    merchantName :: Maybe Text,
    merchantShortId :: Maybe Text,
    issuedByAddress :: Maybe Text,
    supplierName :: Maybe Text,
    supplierGSTIN :: Maybe Text,
    supplierId :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Caller-provided invoice configuration.
--   Everything else (merchant, supplier, currency, entry IDs) comes from FinanceCtx.
data InvoiceConfig = InvoiceConfig
  { invoiceType :: InvoiceType,
    issuedToType :: Text,
    issuedToId :: Text,
    issuedToName :: Maybe Text,
    issuedToAddress :: Maybe Text,
    lineItems :: [InvoiceLineItem],
    gstBreakdown :: Maybe GstAmountBreakdown
  }
  deriving (Eq, Show, Generic)

-- | Accumulated state within a FinanceM computation.
--   Entry IDs from all transfers are collected automatically.
data FinanceState = FinanceState
  { collectedEntryIds :: [Id LE.LedgerEntry]
  }
  deriving (Eq, Show, Generic)

emptyState :: FinanceState
emptyState = FinanceState {collectedEntryIds = []}

-- | Declarative account roles.
--   Instead of calling 10+ separate getOrCreate*Account functions,
--   just say what role you need and the context fills in the details.
data AccountRole
  = -- Wallet flow accounts
    BuyerAsset
  | BuyerExternal
  | OwnerLiability
  | OwnerExpense
  | GovtIndirect
  | GovtDirect
  | PlatformAsset
  | -- Prepaid flow accounts
    PrepaidOwner
  | SellerAsset
  | SellerLiability
  | SellerRideCredit
  | SellerRevenue
  | GovtDirectAsset
  | GovtDirectExpense
  deriving (Eq, Show, Generic)

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
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  BuyerExternal ->
    AccountInput
      { accountType = External,
        counterpartyType = Just BUYER,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  OwnerLiability ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just ctx.counterpartyType,
        counterpartyId = Just ctx.counterpartyId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  OwnerExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just ctx.counterpartyType,
        counterpartyId = Just ctx.counterpartyId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtIndirect ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just GOVERNMENT_INDIRECT,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtDirect ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just GOVERNMENT_DIRECT,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PlatformAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  PrepaidOwner ->
    AccountInput
      { accountType = RideCredit,
        counterpartyType = Just ctx.counterpartyType,
        counterpartyId = Just ctx.counterpartyId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerLiability ->
    AccountInput
      { accountType = Liability,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerRideCredit ->
    AccountInput
      { accountType = RideCredit,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  SellerRevenue ->
    AccountInput
      { accountType = Revenue,
        counterpartyType = Just SELLER,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtDirectAsset ->
    AccountInput
      { accountType = Asset,
        counterpartyType = Just GOVERNMENT_DIRECT,
        counterpartyId = Just ctx.merchantId,
        currency = ctx.currency,
        merchantId = ctx.merchantId,
        merchantOperatingCityId = ctx.merchantOpCityId
      }
  GovtDirectExpense ->
    AccountInput
      { accountType = Expense,
        counterpartyType = Just GOVERNMENT_DIRECT,
        counterpartyId = Just ctx.merchantId,
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
  (BeamFlow.BeamFlow m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m (Maybe (Id LE.LedgerEntry))
transfer fromRole toRole amount refType = do
  if amount <= 0
    then pure Nothing
    else do
      ctx <- ask
      fromAcc <- account fromRole
      toAcc <- account toRole
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAcc.id,
                toAccountId = toAcc.id,
                amount = amount,
                currency = ctx.currency,
                entryType = LE.Expense,
                status = LE.SETTLED,
                referenceType = refType,
                referenceId = ctx.referenceId,
                metadata = Nothing,
                merchantId = ctx.merchantId,
                merchantOperatingCityId = ctx.merchantOpCityId,
                settlementStatus = Nothing
              }
      result <- liftFinanceM (createEntryWithBalanceUpdate entryInput)
      collectEntryId result.id
      pure (Just result.id)

-- | Like 'transfer' but does NOT collect the entry ID.
--   Use for intermediate legs (e.g. BuyerAsset -> BuyerExternal)
--   that should not appear on invoices.
transfer_ ::
  (BeamFlow.BeamFlow m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m ()
transfer_ fromRole toRole amount refType = do
  when (amount > 0) $ do
    ctx <- ask
    fromAcc <- account fromRole
    toAcc <- account toRole
    let entryInput =
          LedgerEntryInput
            { fromAccountId = fromAcc.id,
              toAccountId = toAcc.id,
              amount = amount,
              currency = ctx.currency,
              entryType = LE.Expense,
              status = LE.SETTLED,
              referenceType = refType,
              referenceId = ctx.referenceId,
              metadata = Nothing,
              merchantId = ctx.merchantId,
              merchantOperatingCityId = ctx.merchantOpCityId,
              settlementStatus = Nothing
            }
    _ <- liftFinanceM (createEntryWithBalanceUpdate entryInput)
    pure ()

-- | Like 'transfer' but allows zero-amount entries (e.g. placeholder TDS entries).
--   Skips only for negative amounts.  Automatically collects the entry ID.
transferAllowZero ::
  (BeamFlow.BeamFlow m r) =>
  AccountRole ->
  AccountRole ->
  HighPrecMoney ->
  Text -> -- Reference type
  FinanceM m (Maybe (Id LE.LedgerEntry))
transferAllowZero fromRole toRole amount refType = do
  if amount < 0
    then pure Nothing
    else do
      ctx <- ask
      fromAcc <- account fromRole
      toAcc <- account toRole
      let entryInput =
            LedgerEntryInput
              { fromAccountId = fromAcc.id,
                toAccountId = toAcc.id,
                amount = amount,
                currency = ctx.currency,
                entryType = LE.Expense,
                status = LE.SETTLED,
                referenceType = refType,
                referenceId = ctx.referenceId,
                metadata = Nothing,
                merchantId = ctx.merchantId,
                merchantOperatingCityId = ctx.merchantOpCityId,
                settlementStatus = Nothing
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
invoice :: (BeamFlow.BeamFlow m r) => InvoiceConfig -> FinanceM m ()
invoice config = do
  ctx <- ask
  ids <- getEntryIds
  unless (null ids) $ do
    let invoiceInput =
          InvoiceInput
            { invoiceType = config.invoiceType,
              paymentOrderId = Nothing,
              issuedToType = config.issuedToType,
              issuedToId = config.issuedToId,
              issuedToName = config.issuedToName,
              issuedToAddress = config.issuedToAddress,
              issuedByType = "BUYER",
              issuedById = ctx.merchantId,
              issuedByName = ctx.merchantName,
              issuedByAddress = ctx.issuedByAddress,
              supplierName = ctx.supplierName,
              supplierAddress = ctx.issuedByAddress,
              supplierGSTIN = ctx.supplierGSTIN,
              supplierId = ctx.supplierId,
              gstinOfParty = Nothing,
              panOfParty = Nothing,
              tanOfDeductee = Nothing,
              lineItems = config.lineItems,
              gstBreakdown = config.gstBreakdown,
              currency = ctx.currency,
              dueAt = Nothing,
              merchantId = ctx.merchantId,
              merchantOperatingCityId = ctx.merchantOpCityId,
              merchantShortId = fromMaybe ctx.merchantId ctx.merchantShortId
            }
    _ <- liftFinanceM (createInvoice invoiceInput ids)
    pure ()
