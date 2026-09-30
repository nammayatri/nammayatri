-- | R63 (05 §8.11): a shared-cab booking still FINDING after findingTimeoutSec is cancelled by the system with a full
-- refund, since no cab ever took the rider. Driven from the allocation tick, which already scans the city's FINDING
-- bookings; the decision is the pure `findingTimeoutAction`.
module SharedLogic.SharedCab.FindingTimeout
  ( CancelFlow,
    cancelTimedOutFindings,
    findingTimeoutRefund,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingStatus as DFTBStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSCancelJourney (cancelJourneyById)
import SharedLogic.FRFSUtils (getJourneyIdFromBooking, markFRFSBookingStatus, noPaymentDue)
import qualified SharedLogic.Payment as SPayment
import SharedLogic.SharedCab.Allocation (cityConfig, claimable, clearAllocationKeys)
import SharedLogic.SharedCab.Allocation.Types (FindingTimeout (..), findingTimeoutAction)
import SharedLogic.SharedCab.Booking (withBookingLock)
import qualified SharedLogic.SharedCab.Degraded as Degraded
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.LegState (SharedCabState (FINDING))
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.RefundDecision (Refund (..), refundAmounts)
import SharedLogic.SharedCab.RefundPolicy (CancelBy (..), CancelDecision (..), decideCancel)
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import Tools.Metrics (HasBAPMetrics)
import qualified UrlShortner.Common as UrlShortner

-- | The tick's env: what the payment refund and the rider push need on top of the event and lock flows.
type CancelFlow m r c =
  ( Events.EventFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    EsqDBReplicaFlow m r,
    MonadMask m,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text],
    Finance.HasActorInfo m r
  )

-- | R54 through its own pure decision: a booking cancelled while FINDING is refunded in full, unless a no-show is already
-- booked against it (then the no-show rule wins and the refund is nothing).
findingTimeoutRefund :: Int -> [DFRFSTicket.FRFSTicketStatus] -> Refund
findingTimeoutRefund noShows tickets = case decideCancel ByRider FINDING noShows tickets False of
  Allowed refund -> refund
  Rejected _ -> NoRefund

-- | `live` is the tick's read of the city's live bookings; every candidate is decided again from a fresh read under its
-- booking lock, and a failure on one booking is logged and never stops the others or the tick.
cancelTimedOutFindings ::
  CancelFlow m r c =>
  Id DMOC.MerchantOperatingCity ->
  [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])] ->
  m ()
cancelTimedOutFindings cityId live = do
  cfg <- cityConfig cityId
  now <- getCurrentTime
  forM_ [b | (b, statuses) <- live, isNothing b.vehicleNumber, DFRFSTicket.ACTIVE `elem` statuses, findingTimeoutAction now cfg.findingTimeoutSec b.createdAt == CancelNoCab] $ \b ->
    withTryCatch "sharedCabFindingTimeout" (cancelOne cfg.findingTimeoutSec b)
      >>= either (\e -> logError $ "shared-cab finding-timeout cancel failed for booking " <> b.id.getId <> ": " <> show e) pure

cancelOne ::
  CancelFlow m r c =>
  Int ->
  DFTB.FRFSTicketBooking ->
  m ()
cancelOne findingTimeoutSec stale = do
  now <- getCurrentTime
  -- DB and Redis only under the lock; the refund's network calls come after it
  cancelled <-
    withBookingLock stale.id $
      QFRFSTicketBooking.findById stale.id >>= \case
        Nothing -> pure Nothing
        Just b -> do
          statuses <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id
          markerAlive <- Degraded.isMarkerAlive b.id
          -- the booking may have been claimed, boarded or cancelled since the tick read it
          if claimable b.status b.vehicleNumber statuses markerAlive && findingTimeoutAction now findingTimeoutSec b.createdAt == CancelNoCab
            then do
              let refund = findingTimeoutRefund b.sharedCabNoShows statuses
              Just (b, refund) <$ flipToCancelled refund b
            else pure Nothing
  whenJust cancelled $ \(b, decision) -> do
    when (decision == FullRefund) $ startRefund b
    getJourneyIdFromBooking b >>= mapM_ cancelJourneyById
    Events.forBooking (Events.BookingCancelled "system" (if decision == FullRefund then "full" else "none") (Just "finding_timeout")) b
    Invariants.checkBooking b.id
    if decision == FullRefund then Notify.notifyFindingTimeout b else Notify.notifyBookingCancelled b.sharedCabNoShows b

flipToCancelled ::
  (Events.EventFlow m r, Redis.HedisFlow m r, HasBAPMetrics m r) =>
  Refund ->
  DFTB.FRFSTicketBooking ->
  m ()
flipToCancelled decision b = do
  markFRFSBookingStatus DFTBStatus.CANCELLED "shared_cab_finding_timeout" b
  void $ QFRFSTicket.updateAllStatusByBookingId DFRFSTicket.CANCELLED b.id
  void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCELLED) b.id
  let (charges, refundAmount) = refundAmounts (fromMaybe b.totalPrice.amount b.overriddenAmount) decision
  void $ QFRFSTicketBooking.updateRefundCancellationChargesAndIsCancellableByBookingId (Just refundAmount) (Just charges) (Just True) b.id
  clearAllocationKeys b.id

-- | Starts the refund of the payment; a free (pass-covered) booking has none. A failure is logged and left for ops, since the
-- booking is already CANCELLED and no later tick sees it.
startRefund ::
  CancelFlow m r c =>
  DFTB.FRFSTicketBooking ->
  m ()
startRefund b = do
  mbPayment <- QFRFSTicketBookingPayment.findTicketBookingPayment b
  isFree <- noPaymentDue b
  case mbPayment of
    Just payment ->
      withTryCatch "sharedCabFindingTimeoutRefund" (SPayment.markRefundPendingAndSyncOrderStatus b.merchantId b.riderId payment.paymentOrderId)
        >>= either (\e -> logError $ "shared-cab finding-timeout refund NOT started for booking " <> b.id.getId <> ": " <> show e) (const (pure ()))
    Nothing -> unless isFree . logError $ "shared-cab finding-timeout: booking " <> b.id.getId <> " has no payment to refund"
