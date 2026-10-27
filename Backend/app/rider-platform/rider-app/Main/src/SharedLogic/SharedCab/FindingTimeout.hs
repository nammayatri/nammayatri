-- | A shared-cab booking still FINDING after findingTimeoutSec is cancelled by the system with a full refund, since no cab
-- ever took the rider. Driven from the allocation tick; the decision is the pure `findingTimeoutAction`.
module SharedLogic.SharedCab.FindingTimeout
  ( CancelFlow,
    cancelTimedOutFindings,
    findingTimeoutRefund,
    startRefund,
    maxFindingTimeoutBatch,
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
import Lib.Scheduler (JobCreator)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.FRFSCancelJourney (cancelJourneyIfOnlyTransitLeg)
import SharedLogic.FRFSUtils (markFRFSBookingStatus, noPaymentDue)
import qualified SharedLogic.Payment as SPayment
import SharedLogic.SharedCab.Allocation.Reallocate (releaseCancelledBooking)
import SharedLogic.SharedCab.Allocation.Select (LiveBooking (..), Phase (..), claimable, liveBookingPhase)
import SharedLogic.SharedCab.Allocation.Types (FindingTimeout (..), findingTimeoutAction)
import SharedLogic.SharedCab.Booking (recordCancelReason, withBookingLock)
import SharedLogic.SharedCab.BookingState (clearBookingState, readFindingSince)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.LegState (CancelReason (NO_CAB_FOUND), SharedCabState (FINDING))
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.RefundDecision (Refund (..), gateByPayment, owesRefund, refundAmounts, refundWord)
import SharedLogic.SharedCab.RefundPolicy (CancelBy (..), CancelDecision (..), decideCancel)
import qualified SharedLogic.SharedCab.RefundRetry as RefundRetry
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import Tools.Metrics (HasBAPMetrics)
import qualified UrlShortner.Common as UrlShortner

type CancelFlow m r c =
  ( Events.EventFlow m r,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EncFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    EsqDBReplicaFlow m r,
    MonadMask m,
    JobCreator r m,
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

findingTimeoutRefund :: Int -> [DFRFSTicket.FRFSTicketStatus] -> Refund
findingTimeoutRefund noShows tickets = case decideCancel ByRider FINDING noShows tickets False of
  Allowed refund -> refund
  Rejected _ -> NoRefund

-- | Refunds are sequential network calls inside the tick; a city-wide outage would otherwise hold the tick for candidates x
-- payment latency. A capped batch a tick: the tail stays FINDING and is re-derived from the next tick's `live` scan.
maxFindingTimeoutBatch :: Int
maxFindingTimeoutBatch = 25

cancelTimedOutFindings ::
  CancelFlow m r c =>
  Id DMOC.MerchantOperatingCity ->
  [LiveBooking] ->
  m ()
cancelTimedOutFindings cityId live = do
  cfg <- Config.getTunables cityId
  now <- getCurrentTime
  candidates <- fmap catMaybes . forM [lb.booking | lb <- live, liveBookingPhase lb == Finding] $ \b -> do
    since <- readFindingSince b.id b.createdAt
    pure $ if findingTimeoutAction now cfg.findingTimeoutSec since b.createdAt == CancelNoCab then Just b else Nothing
  let batch = take maxFindingTimeoutBatch candidates
  when (length candidates > length batch) $
    logWarning $ "shared-cab finding-timeout: " <> show (length candidates - length batch) <> " of " <> show (length candidates) <> " candidates deferred to the next tick (batch cap " <> show maxFindingTimeoutBatch <> ")"
  forM_ batch $ \b ->
    withTryCatch "sharedCabFindingTimeout" (cancelOne cfg.findingTimeoutSec b)
      >>= either (\e -> logError $ "shared-cab finding-timeout cancel failed for booking " <> b.id.getId <> ": " <> show e) pure

cancelOne ::
  CancelFlow m r c =>
  Int ->
  DFTB.FRFSTicketBooking ->
  m ()
cancelOne findingTimeoutSec stale = do
  now <- getCurrentTime
  cancelled <-
    withBookingLock stale.id $
      QFRFSTicketBooking.findById stale.id >>= \case
        Nothing -> pure Nothing
        Just b -> do
          statuses <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id
          markerAlive <- Degraded.isMarkerAlive b.id
          since <- readFindingSince b.id b.createdAt
          if claimable b.status b.vehicleNumber statuses markerAlive && findingTimeoutAction now findingTimeoutSec since b.createdAt == CancelNoCab
            then do
              refund <- gateByPayment b (findingTimeoutRefund b.sharedCabNoShows statuses)
              Just (b, refund) <$ flipToCancelled refund b
            else pure Nothing
  whenJust cancelled $ \(b, decision) -> do
    -- each step is its own try: the booking is CANCELLED by now, so one failure must not skip the rest or the rider's push
    refunded <- if owesRefund decision then startRefund b else pure True
    withTryCatch "sharedCab:findingTimeout:releaseCancelledBooking" (releaseCancelledBooking b)
      >>= either (\e -> logError $ "shared-cab finding-timeout release effects failed for booking " <> b.id.getId <> ": " <> show e) pure
    void . withTryCatch "sharedCab:findingTimeout:cancelJourney" $ cancelJourneyIfOnlyTransitLeg b.searchId.getId
    void . withTryCatch "sharedCab:findingTimeout:recordCancelReason" $ recordCancelReason b.id NO_CAB_FOUND
    Events.forBooking (Events.BookingCancelled "system" (refundWord decision) (Just "finding_timeout")) b
    Invariants.checkBooking b.id
    case decision of
      FullRefund -> when refunded $ Notify.notifyFindingTimeout b -- never "refunded in full" to a rider whose refund did not start
      NothingPaid -> Notify.notifyFindingTimeout b
      NoRefund -> Notify.notifyBookingCancelled b.sharedCabNoShows b

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
  clearBookingState b.id
  Seats.releaseBooking b.id

-- | Starts the refund of the payment; a free (pass-covered) booking has none. A failure is logged and left for ops since the
-- booking is already CANCELLED and no later tick sees it, but ALSO marks it for the retry sweep (RefundRetry).
startRefund ::
  CancelFlow m r c =>
  DFTB.FRFSTicketBooking ->
  m Bool
startRefund b = do
  mbPayment <- QFRFSTicketBookingPayment.findTicketBookingPayment b
  isFree <- noPaymentDue b
  case mbPayment of
    Just payment ->
      withTryCatch "sharedCab:findingTimeout:startRefund" (SPayment.markRefundPendingAndSyncOrderStatus b.merchantId b.riderId payment.paymentOrderId)
        >>= either (\e -> False <$ (logError ("shared-cab refund NOT started for booking " <> b.id.getId <> ": " <> show e) >> mark)) (const (pure True))
    Nothing ->
      isFree
        <$ unless
          isFree
          (logError ("shared-cab booking " <> b.id.getId <> " has no payment to refund") >> mark)
  where
    mark = void $ withTryCatch "sharedCabFindingTimeoutRefundMark" (RefundRetry.markRefundRetry b.merchantId b.merchantOperatingCityId b.id)
