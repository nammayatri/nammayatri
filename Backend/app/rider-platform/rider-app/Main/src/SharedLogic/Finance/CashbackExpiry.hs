{-
  Offer cashback redeem window.

  An offer can set 'cashbackRedeemWindowInHours'. The cashback a ride earns from such an
  offer must be redeemed (its payout started) before ride end + window, else it is
  forfeited:

  * At ride end, 'stampCashbackRedeemBy' writes the deadline on the ride's CashbackPayout
    ledger leg (metadataV2.cashbackRedeemBy) and an ExpireOfferCashback job is scheduled
    for the deadline.
  * The cashback payout skips legs past their deadline ('isCashbackRedeemable'), which
    covers the gap between the deadline and the moment the scheduler runs the job.
  * The job ('expireRideCashback') runs under the payout's per-person lock. A leg still
    unsettled is reversed (OwnerLiability → BuyerExpense, the expense is undone) and both
    rows are marked FORFEITED. A PENDING leg (online ride never captured) is voided.
    A PROCESSING leg is an in-flight payout that started in time: the job checks again later.
-}
module SharedLogic.Finance.CashbackExpiry
  ( cashbackRedeemBy,
    stampCashbackRedeemBy,
    isCashbackRedeemable,
    expireRideCashback,
    ExpireOutcome (..),
    cashbackPayoutLockKey,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (partition)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Account as DA
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Ledger.Service as LedgerService
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance

-- | The per-person lock the cashback payout runs under; the expiry job takes the same one,
--   so a payout and a forfeit never run at the same time for one rider.
cashbackPayoutLockKey :: Id DP.Person -> Text
cashbackPayoutLockKey personId = "CashRideCashbackPayoutJob:" <> personId.getId

-- | The redeem deadline for cashback earned at the given time, when the offer sets a window.
cashbackRedeemBy :: Maybe Int -> UTCTime -> Maybe UTCTime
cashbackRedeemBy mbWindowInHours earnedAt = do
  windowInHours <- mbWindowInHours
  guard (windowInHours > 0)
  pure $ addUTCTime (fromIntegral (windowInHours * 3600)) earnedAt

-- | False once a cashback leg is past its redeem deadline; legs without one never expire.
isCashbackRedeemable :: UTCTime -> LE.LedgerEntry -> Bool
isCashbackRedeemable now entry = maybe True (> now) (entry.metadataV2 >>= (.cashbackRedeemBy))

-- | The ride's live CashbackPayout legs: not voided, not a reversal, and not already reversed
--   (a reversed leg nets to zero with its reversal, e.g. one replaced by a reposted leg).
findRideCashbackLegs :: (FinanceBeamFlow.BeamFlow m r) => Id DRide.Ride -> m [LE.LedgerEntry]
findRideCashbackLegs rideId = do
  entries <- filter (\e -> e.status /= LE.VOIDED) <$> LedgerService.getEntriesByReference RidePaymentFinance.ridePaymentRefCashbackPayout rideId.getId
  let reversedIds = mapMaybe (.reversalOf) entries
  pure $ filter (\e -> isNothing e.reversalOf && e.id `notElem` reversedIds) entries

-- | Write the redeem deadline on the ride's cashback legs.
stampCashbackRedeemBy ::
  (FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Id DRide.Ride ->
  UTCTime ->
  m ()
stampCashbackRedeemBy rideId redeemBy = do
  legs <- findRideCashbackLegs rideId
  when (null legs) $
    logError $ "No cashback ledger leg to stamp the redeem deadline on, ride=" <> rideId.getId
  forM_ legs $ \leg -> LedgerService.mergeEntryMetadata leg.id (redeemByMetadata redeemBy)

redeemByMetadata :: UTCTime -> LE.LedgerEntryMetadata
redeemByMetadata redeemBy =
  LE.LedgerEntryMetadata
    { d2cReferralEarnings = Nothing,
      d2dReferralEarnings = Nothing,
      dailyStatsId = Nothing,
      driverPayable = Nothing,
      payoutOrderId = Nothing,
      reason = Nothing,
      subscriptionAllocations = Nothing,
      cashbackRedeemBy = Just redeemBy
    }

data ExpireOutcome
  = -- | Nothing open to forfeit (already paid out, already forfeited, or no leg).
    NothingToExpire
  | -- | The forfeited amount.
    Forfeited HighPrecMoney
  | -- | A payout that started before the deadline is still in flight.
    StillProcessing
  deriving (Show, Eq)

forfeitReason :: Text
forfeitReason = "OFFER_CASHBACK_EXPIRED"

-- | Forfeit the ride's cashback that is still unpaid at its deadline, under the payout lock.
expireRideCashback ::
  (FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r, CacheFlow m r, MonadFlow m) =>
  Id DP.Person ->
  Id DRide.Ride ->
  m ExpireOutcome
expireRideCashback personId rideId = do
  outcomeRef <- liftIO $ newIORef NothingToExpire
  PayoutRequest.runPayoutUnderLock (cashbackPayoutLockKey personId) 120 (Just <$> forfeitRideCashback personId rideId) (liftIO . writeIORef outcomeRef)
  liftIO $ readIORef outcomeRef

forfeitRideCashback ::
  (FinanceBeamFlow.BeamFlow m r, Finance.HasActorInfo m r, MonadFlow m) =>
  Id DP.Person ->
  Id DRide.Ride ->
  m ExpireOutcome
forfeitRideCashback personId rideId = do
  mbWallet <- RidePaymentFinance.getWalletAccountByOwner DA.RIDER personId.getId
  case mbWallet of
    Nothing -> do
      logInfo $ "No rider wallet, no cashback to expire; person=" <> personId.getId <> " ride=" <> rideId.getId
      pure NothingToExpire
    Just wallet -> do
      legs <- filter (\e -> e.toAccountId == wallet.id) <$> findRideCashbackLegs rideId
      let isOpen e = e.settlementStatus `elem` [Nothing, Just LE.UNSETTLED]
          inFlight = filter (\e -> e.settlementStatus == Just LE.PROCESSING) legs
          (settled, pending) = partition (\e -> e.status == LE.SETTLED) (filter isOpen legs)
          settlementId = "cashback-expiry:" <> rideId.getId
      if not (null inFlight)
        then do
          logInfo $ "Cashback payout in flight at the redeem deadline, checking again later; ride=" <> rideId.getId
          pure StillProcessing
        else do
          -- Never captured (online ride): it moved no money, so it is voided, not reversed.
          forM_ pending $ \e -> LedgerService.voidEntry e.id forfeitReason
          -- The reversal is born FORFEITED, so no payout ever nets it; then the original is marked.
          forM_ settled $ \e ->
            LedgerService.createReversalWith e.id forfeitReason (Just (LE.FORFEITED, settlementId)) >>= \case
              Left err -> throwError . InternalError $ "Cashback forfeit reversal failed for entry " <> e.id.getId <> ": " <> show err
              Right reversal -> LedgerService.markEntriesAsForfeited [e.id, reversal.id] settlementId
          let total = sum (map (.amount) settled) + sum (map (.amount) pending)
          if total > 0
            then do
              logInfo $ "Forfeited offer cashback " <> show total <> " person=" <> personId.getId <> " ride=" <> rideId.getId
              pure (Forfeited total)
            else pure NothingToExpire
