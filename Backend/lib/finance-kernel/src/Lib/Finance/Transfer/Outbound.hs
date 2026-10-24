{-
  Lib.Finance.Transfer.Outbound

  FA -> real account (payout). One orchestration for every rail: config gates, amount
  resolution under the balance lock, the hold <-> submit ordering the rail declares,
  and a uniform compensation ladder. Replaces the per-flow initiate logic in the
  driver wallet (sendThenHold / holdThenSend) and rider cashback.

  Contract: after the Transfer row exists, this function never returns Left for a
  money-flow problem — the row's status and lastError carry the story (FAILED,
  NEEDS_REVIEW, ...). Left is reserved for pre-row rejections (disabled class,
  insufficient balance, daily cap) and infrastructure errors.

  Design: docs/backend/design/finance-kernel-unification-plan.md §5.3.
-}
module Lib.Finance.Transfer.Outbound
  ( OutboundSpec (..),
    initiateTransferOut,
  )
where

import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Balance
import Lib.Finance.Domain.Types.Account (Account (..))
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.Error.Types
import Lib.Finance.FinanceM (AccountRole (..), FinanceCtx (..), transferInProcessing)
import Lib.Finance.Ledger.PayoutSettlement (PayoutLedgerRefs (..), PayoutOutcome (..), RunFinance, settlePayoutLedger)
import Lib.Finance.Ledger.Service (markEntriesAsProcessing, markEntriesAsUnsettled)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Transfer as QT
import qualified Lib.Finance.Storage.Queries.TransferExtra as QTE
import Lib.Finance.Transfer.Rail

data OutboundSpec = OutboundSpec
  { -- | ctx.referenceId MUST equal 'referenceId' — every ledger leg is keyed on it
    ctx :: FinanceCtx,
    -- | the source finance account (resolved by the domain)
    account :: Account,
    amount :: TransferAmount,
    referenceType :: Text,
    -- | domain request id; (referenceType, referenceId) is the idempotency anchor
    referenceId :: Text,
    -- | hold / settlement reference types + settlement destination role
    ledgerRefs :: PayoutLedgerRefs,
    -- | checked against instantTransferOutEnabled
    instant :: Bool,
    -- | domain-named Redis offer-hold keys still in use (see Lib.Finance.Balance)
    legacyOfferHoldKeys :: [Text],
    -- | earning entries this transfer settles; Nothing = the breakdown's redeemables
    coveredEntryIds :: Maybe [Id LE.LedgerEntry],
    -- | metadata stamped on the hold ledger leg (e.g. driverPayable attribution)
    holdLedgerMetadata :: Maybe LE.LedgerEntryMetadata,
    metadata :: DT.TransferMetadata
  }
  deriving (Generic)

initiateTransferOut ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  TransferRail m ->
  OutboundSpec ->
  m (Either FinanceError DT.Transfer)
initiateTransferOut runFinance rail spec
  | spec.ctx.referenceId /= spec.referenceId =
    pure $ Left $ DomainError "CTX_REFERENCE_MISMATCH" ("ctx " <> spec.ctx.referenceId <> " vs spec " <> spec.referenceId)
  | otherwise = withAccountBalanceLock spec.account.id 10 $ do
    QT.findByReference spec.referenceType spec.referenceId >>= \case
      Just existing -> pure (Right existing) -- idempotent re-entry
      Nothing -> do
        breakdown <- getBalanceBreakdown spec.account spec.legacyOfferHoldKeys
        let cfg = breakdown.config
        gateRes <- runGates cfg
        case gateRes >>= \() -> resolveTransferAmount cfg breakdown spec.amount of
          Left err -> pure (Left err)
          Right resolved -> do
            let covered = fromMaybe (map (.id) breakdown.redeemableEntries) spec.coveredEntryIds
            transfer <- createRow resolved covered
            markEntriesAsProcessing covered (Just transfer.id.getId)
            orchestrate transfer resolved covered
  where
    runGates cfg = do
      dailyCount <- countTowardDailyCap
      pure $ do
        unless cfg.transferOutEnabled $
          Left $ DomainError "TRANSFER_OUT_DISABLED" (show spec.account.id)
        when (spec.instant && not cfg.instantTransferOutEnabled) $
          Left $ DomainError "INSTANT_TRANSFER_NOT_ALLOWED" (show spec.account.id)
        -- The per-day cap gates only instant (user-initiated) transfers; scheduled
        -- sweeps are exempt. All outbound transfers count toward the cap, though —
        -- same semantics the driver wallet enforced before the kernel owned this.
        when spec.instant $
          whenJust_ cfg.maxTransferOutPerDay $ \cap' ->
            when (dailyCount >= cap') $
              Left $ DomainError "TRANSFER_DAILY_LIMIT_REACHED" (show dailyCount <> "/" <> show cap')
      where
        whenJust_ mb f = maybe (Right ()) f mb

    -- Transfers created since UTC midnight; never-sent failures (FAILED without a rail
    -- order id) don't count, mirroring today's reversal-prefix discounting.
    countTowardDailyCap = do
      now <- getCurrentTime
      let dayStart = Time.UTCTime (Time.utctDay now) 0
      rows <- QT.findAllByAccountId spec.account.id
      pure $
        length
          [ t | t <- rows, t.direction == DT.OUTBOUND, t.createdAt >= dayStart, not (t.status == DT.FAILED && isNothing t.railOrderId)
          ]

    createRow resolved covered = do
      transferId <- generateGUID
      now <- getCurrentTime
      let transfer =
            DT.Transfer
              { id = Id transferId,
                direction = DT.OUTBOUND,
                status = DT.CREATED,
                railKey = Just rail.railKey,
                railStatus = Nothing,
                railOrderId = Nothing,
                accountId = spec.account.id,
                counterAccountId = Nothing,
                requestedAmount = case spec.amount of
                  AllAvailable -> Nothing
                  Exact amt -> Just amt,
                amount = resolved.grossDebit,
                feeAmount = resolved.fee,
                currency = spec.account.currency,
                referenceType = spec.referenceType,
                referenceId = spec.referenceId,
                holdEntryId = Nothing,
                entryIds = Just [],
                coveredEntryIds = Just (map (.getId) covered),
                metadata = Just spec.metadata,
                scheduledFor = Nothing,
                attemptCount = 0,
                lastError = Nothing,
                merchantId = spec.account.merchantId,
                merchantOperatingCityId = spec.account.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QT.create transfer
      pure transfer

    submission transfer resolved =
      RailSubmission
        { transferId = transfer.id,
          netAmount = resolved.netToCounterparty,
          grossAmount = resolved.grossDebit,
          fee = resolved.fee,
          currency = transfer.currency,
          accountId = spec.account.id,
          counterpartyType = fromMaybe spec.ctx.counterpartyType spec.account.counterpartyType,
          counterpartyId = fromMaybe spec.ctx.counterpartyId spec.account.counterpartyId,
          referenceType = spec.referenceType,
          referenceId = spec.referenceId,
          metadata = spec.metadata,
          merchantId = spec.account.merchantId,
          merchantOperatingCityId = spec.account.merchantOperatingCityId
        }

    postHold transfer resolved = do
      res <-
        runFinance spec.ctx $
          void $ transferInProcessing OwnerLiability OwnerPayoutLiability resolved.grossDebit spec.ledgerRefs.holdReferenceType spec.holdLedgerMetadata
      case res of
        Left err -> pure (Left err)
        Right ((), entryIds) -> do
          let holdId = listToMaybe entryIds
          QTE.updateLedgerLegsById ((.getId) <$> holdId) (map (.getId) entryIds) transfer.id
          pure (Right entryIds)

    safeSubmit transfer resolved =
      try @_ @SomeException (rail.submit (submission transfer resolved)) <&> \case
        Left exc -> RailAmbiguous (show exc)
        Right r -> r

    -- reverse the hold + release covered entries, then mark FAILED with a coded reason
    compensate transfer covered code = do
      giveBack <- settlePayoutLedger runFinance spec.ledgerRefs spec.ctx transfer.amount spec.holdLedgerMetadata (PayoutFailed code)
      case giveBack of
        Right _ -> do
          markEntriesAsUnsettled covered
          QTE.updateStatusById DT.FAILED Nothing (Just code) transfer.id
        Left err ->
          -- hold exists but could not be reversed: ops must resolve, never silently drop
          QTE.updateStatusById DT.NEEDS_REVIEW Nothing (Just ("REVERSAL_PENDING: " <> code <> "; " <> show err)) transfer.id

    orchestrate transfer resolved covered = do
      case rail.holdOrdering of
        HoldThenSubmit ->
          postHold transfer resolved >>= \case
            Left err -> do
              markEntriesAsUnsettled covered
              QTE.updateStatusById DT.FAILED Nothing (Just ("HOLD_FAILED: " <> show err)) transfer.id
            Right _ -> do
              result <- safeSubmit transfer resolved
              recordSubmit transfer covered result
        SubmitThenHold -> do
          result <- safeSubmit transfer resolved
          case result of
            RailRejected reason -> do
              -- nothing was sent and no hold exists yet: plain failure
              markEntriesAsUnsettled covered
              QTE.updateStatusById DT.FAILED Nothing (Just reason) transfer.id
            other -> do
              -- the partner may have the money in motion: always park it
              holdRes <- postHold transfer resolved
              case holdRes of
                Left err ->
                  QTE.updateStatusById DT.NEEDS_REVIEW Nothing (Just ("HOLD_FAILED_AFTER_SUBMIT: " <> show err)) transfer.id
                Right _ -> recordSubmit transfer covered other
      maybe (Left $ DomainError "TRANSFER_NOT_FOUND" transfer.id.getId) Right <$> QT.findById transfer.id

    recordSubmit transfer covered = \case
      RailAccepted oid rawStatus ->
        QTE.updateSubmissionById DT.IN_FLIGHT (Just oid) (Just rawStatus) 1 transfer.id
      RailDeferred rawStatus ->
        QTE.updateSubmissionById DT.SUBMITTED Nothing (Just rawStatus) 1 transfer.id
      RailAmbiguous reason ->
        QTE.updateStatusById DT.NEEDS_REVIEW Nothing (Just reason) transfer.id
      RailRejected reason ->
        compensate transfer covered ("NOT_SENT: " <> reason)
