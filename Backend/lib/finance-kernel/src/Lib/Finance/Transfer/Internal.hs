{-
  Lib.Finance.Transfer.Internal

  FA -> FA: instant settled entries, recorded as a SUCCEEDED Transfer row so account
  history shows every kind of movement through one feed. No rail, no fee, no
  transfer-out gating (money never leaves the system); only availability is enforced,
  under the from-account's balance lock.
-}
module Lib.Finance.Transfer.Internal
  ( InternalTransferSpec (..),
    transferInternal,
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Balance
import Lib.Finance.Core.Types (HasActorInfo)
import Lib.Finance.Domain.Types.Account (Account (..))
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.Error.Types
import Lib.Finance.FinanceM (AccountRole, FinanceCtx (..), transfer)
import Lib.Finance.Ledger.PayoutSettlement (RunFinance)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Transfer as QT

data InternalTransferSpec = InternalTransferSpec
  { -- | ctx.referenceId MUST equal 'referenceId'
    ctx :: FinanceCtx,
    -- | the source finance account (balance-checked side)
    fromAccount :: Account,
    fromRole :: AccountRole,
    toRole :: AccountRole,
    amount :: TransferAmount,
    referenceType :: Text,
    referenceId :: Text,
    legacyOfferHoldKeys :: [Text],
    ledgerMetadata :: Maybe LE.LedgerEntryMetadata,
    metadata :: DT.TransferMetadata
  }
  deriving (Generic)

transferInternal ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  InternalTransferSpec ->
  m (Either FinanceError DT.Transfer)
transferInternal runFinance spec
  | spec.ctx.referenceId /= spec.referenceId =
    pure $ Left $ DomainError "CTX_REFERENCE_MISMATCH" ("ctx " <> spec.ctx.referenceId <> " vs spec " <> spec.referenceId)
  | otherwise = withAccountBalanceLock spec.fromAccount.id 10 $ do
    QT.findByReference spec.referenceType spec.referenceId >>= \case
      Just existing -> pure (Right existing)
      Nothing -> do
        breakdown <- getBalanceBreakdown spec.fromAccount spec.legacyOfferHoldKeys
        let resolvedE = case spec.amount of
              AllAvailable -> Right breakdown.available
              Exact amt
                | amt <= 0 -> Left $ DomainError "INVALID_TRANSFER_AMOUNT" (show amt)
                | amt > breakdown.available -> Left $ AccountError InsufficientBalance ("requested " <> show amt <> ", available " <> show breakdown.available)
                | otherwise -> Right amt
        case resolvedE of
          Left err -> pure (Left err)
          Right gross
            | gross <= 0 -> pure $ Left $ DomainError "INVALID_TRANSFER_AMOUNT" "nothing available to move"
            | otherwise -> do
              postRes <- runFinance spec.ctx $ void $ transfer spec.fromRole spec.toRole gross spec.referenceType spec.ledgerMetadata
              case postRes of
                Left err -> pure (Left err)
                Right ((), entryIds) -> do
                  transferId <- generateGUID
                  now <- getCurrentTime
                  let row =
                        DT.Transfer
                          { id = Id transferId,
                            direction = DT.INTERNAL,
                            status = DT.SUCCEEDED,
                            railKey = Nothing,
                            railStatus = Nothing,
                            railOrderId = Nothing,
                            accountId = spec.fromAccount.id,
                            counterAccountId = Nothing,
                            requestedAmount = case spec.amount of
                              AllAvailable -> Nothing
                              Exact amt -> Just amt,
                            amount = gross,
                            feeAmount = 0,
                            currency = spec.fromAccount.currency,
                            referenceType = spec.referenceType,
                            referenceId = spec.referenceId,
                            holdEntryId = Nothing,
                            entryIds = Just (map (.getId) entryIds),
                            coveredEntryIds = Just [],
                            metadata = Just spec.metadata,
                            scheduledFor = Nothing,
                            attemptCount = 0,
                            lastError = Nothing,
                            merchantId = spec.fromAccount.merchantId,
                            merchantOperatingCityId = spec.fromAccount.merchantOperatingCityId,
                            createdAt = now,
                            updatedAt = now
                          }
                  QT.create row
                  pure (Right row)
