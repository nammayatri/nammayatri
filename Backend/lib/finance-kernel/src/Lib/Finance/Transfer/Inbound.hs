{-
  Lib.Finance.Transfer.Inbound

  Real account -> FA (recharge / topup / collected payment). The initiate half mints the
  Transfer row and returns the rail's collect payload (SDK payload) for the client; the
  outcome half posts the success legs declared by a CreditRecipe — so webhook handlers
  stop hand-coding credits and the posting is idempotent by construction.

  The recipe is passed again at outcome time (the webhook handler knows its flow); it is
  deliberately NOT stored on the row, so InvoiceConfig and account roles never need to
  be serialized.

  Design: docs/backend/design/finance-kernel-unification-plan.md §5.3.
-}
module Lib.Finance.Transfer.Inbound
  ( CreditRecipe (..),
    CreditLeg (..),
    AmountPortion (..),
    InboundSpec (..),
    initiateTransferIn,
    applyInboundTransferOutcome,
  )
where

import qualified Data.Aeson as Aeson
import Data.List (nub)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.AccountConfig as AccountConfig
import Lib.Finance.Balance (withAccountBalanceLock)
import Lib.Finance.Domain.Types.Account (Account)
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.Error.Types
import Lib.Finance.FinanceM (AccountRole, FinanceCtx (..), InvoiceConfig, invoice, transfer)
import Lib.Finance.Ledger.PayoutSettlement (RunFinance)
import Lib.Finance.Ledger.Service (getEntriesByReference)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.Transfer as QT
import qualified Lib.Finance.Storage.Queries.TransferExtra as QTE
import Lib.Finance.Transfer.Outcome (bumpNonLedgerStatus, guardCtxMatchesTransfer)
import Lib.Finance.Transfer.Rail

-- | How much of the transfer a leg carries. At most one Remainder per recipe; portions
--   must not exceed the total.
data AmountPortion = FullAmount | Portion HighPrecMoney | Remainder
  deriving (Eq, Show, Generic)

data CreditLeg = CreditLeg
  { fromRole :: AccountRole,
    toRole :: AccountRole,
    portion :: AmountPortion,
    referenceType :: Text
  }
  deriving (Generic)

-- | What to post when the money has actually arrived. The first leg's referenceType is
--   the idempotency anchor: if entries already exist under (it, ctx.referenceId), the
--   whole recipe is considered posted.
data CreditRecipe = CreditRecipe
  { legs :: NonEmpty CreditLeg,
    invoiceConfig :: Maybe InvoiceConfig
  }
  deriving (Generic)

data InboundSpec = InboundSpec
  { ctx :: FinanceCtx,
    -- | inbound is always exact — the payer chose the amount
    amount :: HighPrecMoney,
    referenceType :: Text,
    accountId :: Id Account,
    metadata :: DT.TransferMetadata
  }
  deriving (Generic)

-- | The WHOLE recharge process, kernel-owned: gate on the class's transferInEnabled,
--   reuse the open intent when the amount matches (the rail re-fetches that order's
--   payload), supersede stale open intents, otherwise have the rail create a fresh
--   payment order — its id becomes (referenceId, railOrderId), which is what the
--   webhook later correlates on. The domain contributes only the rail closure and
--   which account to credit; it never creates payment orders or tracks intent status.
initiateTransferIn ::
  (BeamFlow.BeamFlow m r) =>
  TransferRail m ->
  InboundSpec ->
  m (Either FinanceError (DT.Transfer, Aeson.Value))
initiateTransferIn rail spec
  | spec.amount <= 0 =
    pure $ Left $ DomainError "INVALID_TRANSFER_AMOUNT" (show spec.amount)
  | otherwise = case rail.collectPayload of
    Nothing -> pure $ Left $ DomainError "INBOUND_RAIL_WITHOUT_COLLECT" rail.railKey
    Just collect -> withAccountBalanceLock spec.accountId 10 $ do
      QAccount.findById spec.accountId >>= \case
        Nothing -> pure $ Left $ AccountError AccountNotFound (show spec.accountId)
        Just account -> do
          cfg <- AccountConfig.getAccountConfigForAccount account
          if not cfg.transferInEnabled
            then pure $ Left $ DomainError "TRANSFER_IN_DISABLED" (show spec.accountId)
            else do
              transferId <- Id <$> generateGUID
              transfers <- QT.findAllByAccountId spec.accountId
              let openIntents =
                    [ t | t <- transfers, t.direction == DT.INBOUND, t.referenceType == spec.referenceType, t.status `elem` [DT.CREATED, DT.SUBMITTED, DT.IN_FLIGHT]
                    ]
                  mbReuse = listToMaybe [t | t <- openIntents, t.amount == spec.amount]
              -- Exactly one live intent per (account, referenceType): anything open that
              -- is not being reused is superseded (the old EXPIRED semantics).
              forM_ [t | t <- openIntents, Just t.id /= ((.id) <$> mbReuse)] $ \old ->
                QTE.updateStatusById DT.FAILED Nothing (Just "SUPERSEDED_BY_NEW_INTENT") old.id
              result <-
                collect
                  RailCollectRequest
                    { submission = toSubmission (maybe transferId (.id) mbReuse) ((.referenceId) <$> mbReuse),
                      reuseRailOrderId = (.referenceId) <$> mbReuse
                    }
              case mbReuse of
                Just existing -> pure $ Right (existing, result.payload)
                Nothing -> do
                  -- A crash between collect and this insert leaves an orphan payment
                  -- order; the webhook backfill covers a payment landing on it, and the
                  -- next initiate simply mints a fresh order.
                  now <- getCurrentTime
                  let row =
                        DT.Transfer
                          { id = transferId,
                            direction = DT.INBOUND,
                            status = DT.CREATED,
                            railKey = Just rail.railKey,
                            railStatus = Nothing,
                            railOrderId = Just result.railOrderId,
                            accountId = spec.accountId,
                            counterAccountId = Nothing,
                            requestedAmount = Just spec.amount,
                            amount = spec.amount,
                            feeAmount = 0,
                            currency = spec.ctx.currency,
                            referenceType = spec.referenceType,
                            referenceId = result.railOrderId,
                            holdEntryId = Nothing,
                            entryIds = Just [],
                            coveredEntryIds = Just [],
                            metadata = Just spec.metadata,
                            scheduledFor = Nothing,
                            attemptCount = 0,
                            lastError = Nothing,
                            merchantId = spec.ctx.merchantId,
                            merchantOperatingCityId = spec.ctx.merchantOpCityId,
                            createdAt = now,
                            updatedAt = now
                          }
                  QT.create row
                  pure $ Right (row, result.payload)
  where
    -- For a fresh intent referenceId is unknown until the rail mints the order; it is
    -- only populated on reuse. Collect closures must key on amount + reuseRailOrderId.
    toSubmission transferId mbRefId =
      RailSubmission
        { transferId = transferId,
          netAmount = spec.amount,
          grossAmount = spec.amount,
          fee = 0,
          currency = spec.ctx.currency,
          counterpartyType = spec.ctx.counterpartyType,
          counterpartyId = spec.ctx.counterpartyId,
          accountId = spec.accountId,
          referenceType = spec.referenceType,
          referenceId = fromMaybe "" mbRefId,
          metadata = spec.metadata,
          merchantId = spec.ctx.merchantId,
          merchantOperatingCityId = spec.ctx.merchantOpCityId
        }

-- | Apply what the rail said about an inbound transfer. On OutcomeSucceeded the recipe
--   legs (+ invoice) are posted exactly once; everything else is a status move.
applyInboundTransferOutcome ::
  (BeamFlow.BeamFlow m r, Redis.HedisFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  FinanceCtx ->
  CreditRecipe ->
  Id DT.Transfer ->
  Maybe Text -> -- raw rail status, verbatim
  TransferOutcome ->
  m (Either FinanceError DT.Transfer)
applyInboundTransferOutcome runFinance ctx recipe transferId mbRaw outcome =
  QT.findById transferId >>= \case
    Nothing -> pure $ Left $ DomainError "TRANSFER_NOT_FOUND" transferId.getId
    Just t -> case guardCtxMatchesTransfer ctx t of
      Left err -> pure (Left err)
      Right ()
        | t.direction /= DT.INBOUND -> pure $ Left $ DomainError "NOT_AN_INBOUND_TRANSFER" transferId.getId
        | otherwise -> case outcome of
          OutcomeSucceeded -> settleInbound t mbRaw
          other -> bumpNonLedgerStatus t mbRaw other
  where
    anchorRefType = let (firstLeg :| _) = recipe.legs in firstLeg.referenceType

    settleInbound t mbRaw'
      | t.status == DT.SUCCEEDED = pure (Right t)
      | otherwise = do
        alreadyPosted <- not . null <$> getEntriesByReference anchorRefType ctx.referenceId
        postedIds <-
          if alreadyPosted
            then pure (Right [])
            else case resolvePortions t.amount (toList recipe.legs) of
              Left err -> pure (Left err)
              Right legAmounts ->
                fmap snd
                  <$> runFinance
                    ctx
                    ( do
                        forM_ legAmounts $ \(leg, legAmount) ->
                          void $ transfer leg.fromRole leg.toRole legAmount leg.referenceType Nothing
                        forM_ recipe.invoiceConfig $ \cfgInv ->
                          void $ invoice cfgInv
                    )
        case postedIds of
          Left err -> pure (Left err)
          Right entryIds -> do
            QTE.updateLedgerLegsById Nothing (nub (fromMaybe [] t.entryIds <> map (.getId) entryIds)) t.id
            QTE.updateStatusById DT.SUCCEEDED mbRaw' Nothing t.id
            maybe (Left $ DomainError "TRANSFER_NOT_FOUND" t.id.getId) Right <$> QT.findById t.id

-- | Turn portions into concrete amounts. FullAmount legs carry the whole total (several
--   are allowed — e.g. a pass-through pair); Portion/Remainder split it.
resolvePortions :: HighPrecMoney -> [CreditLeg] -> Either FinanceError [(CreditLeg, HighPrecMoney)]
resolvePortions total legs = do
  let fixedSum = sum [p | CreditLeg {portion = Portion p} <- legs]
      remainders = length [() | CreditLeg {portion = Remainder} <- legs]
  when (remainders > 1) $ Left $ DomainError "INVALID_CREDIT_RECIPE" "more than one Remainder leg"
  when (fixedSum > total) $ Left $ DomainError "INVALID_CREDIT_RECIPE" ("portions " <> show fixedSum <> " exceed total " <> show total)
  pure $
    legs <&> \leg ->
      ( leg,
        case leg.portion of
          FullAmount -> total
          Portion p -> p
          Remainder -> total - fixedSum
      )
