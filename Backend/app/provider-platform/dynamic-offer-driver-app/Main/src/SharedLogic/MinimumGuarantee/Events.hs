{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Event handlers of Minimum Guarantee (MG) campaigns. Each hook (ride ended, driver-fault
-- cancellation, driver mode change, fleet association change) calls one function here. For a driver
-- with no MG day the cost is a single Redis read; otherwise the driver's enrollment rows are
-- updated under a per-driver lock.
--
-- Hooks must never break the flow they sit in, so callers wrap these in 'withTryCatch'.
module SharedLogic.MinimumGuarantee.Events
  ( onRideEnded,
    onDriverAtFaultCancellation,
    onDriverModeChanged,
    checkFleetChangeAllowed,
    onFleetChanged,
  )
where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Extra.MgEnrollment as DEME
import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgCampaignRule as DMgR
import qualified Domain.Types.MgEnrollment as DMgE
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.MinimumGuarantee.Budget (releaseBudget)
import SharedLogic.MinimumGuarantee.Context
import SharedLogic.MinimumGuarantee.Rules
import qualified Storage.Queries.MgCampaign as QMgC
import qualified Storage.Queries.MgCampaignRule as QMgR
import qualified Storage.Queries.MgEnrollment as QMgE
import qualified Storage.Queries.Person as QP
import qualified Tools.Notifications as Notify

-- | Runs `f` for each of the driver's live MG days (enrollment still UPCOMING, campaign published
-- or paused), under the driver lock.
withLiveDays ::
  MgEventFlow m r =>
  Id DP.Person ->
  (MgDayCtx -> DMgE.MgEnrollment -> DMgC.MgCampaign -> [DMgR.MgCampaignRule] -> m ()) ->
  m ()
withLiveDays driverId f = do
  ctxs <- getMgCtxs driverId
  unless (null ctxs) $
    withMgDriverLock driverId $
      forM_ ctxs $ \ctx -> do
        mbEnrollment <- QMgE.findByPrimaryKey ctx.enrollmentId
        mbCampaign <- QMgC.findByPrimaryKey ctx.campaignId
        case (mbEnrollment, mbCampaign) of
          (Just enrollment, Just campaign)
            | enrollment.status == DMgE.UPCOMING -> do
              rules <- QMgR.findAllByCampaignId campaign.id
              f ctx enrollment campaign rules
          _ -> pure ()

-- | Newest last, capped so a misbehaving day cannot grow the row without bound.
appendEvent :: UTCTime -> DMgE.MgEventType -> Maybe Text -> Maybe Int -> [DMgE.MgEvent] -> [DMgE.MgEvent]
appendEvent now eventType detail percentAfter events =
  reverse . take maxEvents . reverse $
    events <> [DMgE.MgEvent {occurredAt = now, eventType, detail, payoutPercentAfter = percentAfter}]
  where
    maxEvents = 100

-- | Stores new metrics with the recomputed payout percent and an audit entry. A limit rule that
-- reaches 0% cannot recover, so the day is disqualified on the spot and leaves the Redis entry,
-- which stops force-assign and any further events for it.
applyMetrics ::
  MgEventFlow m r =>
  UTCTime ->
  [DMgR.MgCampaignRule] ->
  MgDayCtx ->
  DMgE.MgEnrollment ->
  DEME.MgMetrics ->
  DMgE.MgEventType ->
  Maybe Text ->
  m ()
applyMetrics now rules ctx enrollment metrics' eventType detail = do
  let percent = payoutPercentFor rules metrics'
      exhausted = limitRuleExhausted rules metrics'
      events1 = appendEvent now eventType detail (Just percent) enrollment.events
      events2 = if exhausted then appendEvent now DMgE.DAY_DISQUALIFIED Nothing (Just 0) events1 else events1
  QMgE.updateByPrimaryKey
    enrollment
      { DMgE.metrics = metrics',
        DMgE.payoutPercent = Just percent,
        DMgE.status = if exhausted then DMgE.DISQUALIFIED else enrollment.status,
        DMgE.events = events2
      }
  when exhausted $ do
    unregisterMgCtx enrollment.driverId ctx.enrollmentId
    notifyDriverOfDisqualification enrollment.driverId

notifyDriverOfDisqualification :: MgEventFlow m r => Id DP.Person -> m ()
notifyDriverOfDisqualification driverId = do
  mbDriver <- QP.findById driverId
  whenJust mbDriver $ \driver ->
    Notify.notifyDriver
      driver.merchantOperatingCityId
      FCM.DRIVER_NOTIFY
      "Minimum guarantee"
      "You are no longer eligible for today's minimum guarantee."
      driver
      Nothing

-- | A completed ride counts towards the day when it started inside a window, even if it ended
-- after the window. Counting is idempotent per ride id.
onRideEnded :: MgEventFlow m r => DRide.Ride -> m ()
onRideEnded ride =
  whenJust ride.tripStartTime $ \startedAt ->
    withLiveDays ride.driverId $ \ctx enrollment _campaign rules ->
      when (insideAnySpan startedAt ctx.spans) $ do
        now <- getCurrentTime
        let fare = fromMaybe 0 ride.fare
            distanceMeters = maybe 0 getMeters ride.chargeableDistance
            metrics = enrollment.metrics
            metrics' = addCountedRide rules ride.id.getId fare distanceMeters metrics
            fareText = show (roundToIntegral (getHighPrecMoney fare) :: Int)
        unless (metrics' == metrics) $
          applyMetrics now rules ctx enrollment metrics' DMgE.RIDE_COUNTED (Just ("ride " <> ride.id.getId <> ", fare " <> fareText))

-- | A cancellation judged to be the driver's fault, inside a window of a running (not paused)
-- campaign, adds one to the day's cancellation count.
onDriverAtFaultCancellation :: MgEventFlow m r => Id DP.Person -> Id DRide.Ride -> m ()
onDriverAtFaultCancellation driverId rideId = do
  now <- getCurrentTime
  withLiveDays driverId $ \ctx enrollment campaign rules ->
    when (campaign.status == DMgC.PUBLISHED && insideAnySpan now ctx.spans) $ do
      let metrics = enrollment.metrics
          metrics' = metrics {DEME.cancellations = metrics.cancellations + 1}
      applyMetrics now rules ctx enrollment metrics' DMgE.DRIVER_CANCELLED (Just ("ride " <> rideId.getId))

-- | A driver-initiated mode change. Going offline or silent starts a break; coming back online
-- adds the part of the break that fell inside the windows. Only the driver-facing set-activity
-- path calls this, so offline writes forced by the system or an admin are never counted.
onDriverModeChanged :: MgEventFlow m r => Id DP.Person -> Maybe DTC.DriverMode -> Maybe DTC.DriverMode -> m ()
onDriverModeChanged driverId oldMode newMode =
  when (isBreak newMode /= isBreak oldMode) $ do
    now <- getCurrentTime
    withLiveDays driverId $ \ctx enrollment campaign rules ->
      when (campaign.status == DMgC.PUBLISHED) $ do
        let metrics = enrollment.metrics
        if isBreak newMode
          then
            when (isNothing metrics.breakStartedAt) $
              applyMetrics now rules ctx enrollment (metrics {DEME.breakStartedAt = Just now}) DMgE.BREAK_STARTED Nothing
          else do
            let metrics' = closeOpenBreak now ctx.spans metrics
                countedMinutes = (metrics'.breakSeconds - metrics.breakSeconds) `div` 60
            unless (metrics' == metrics) $
              applyMetrics
                now
                rules
                ctx
                enrollment
                metrics'
                DMgE.BREAK_ENDED
                (Just (show countedMinutes <> " min counted, total " <> show (metrics'.breakSeconds `div` 60) <> " min"))
  where
    isBreak mode = mode `elem` [Nothing, Just DTC.OFFLINE, Just DTC.SILENT]

-- | Before a fleet association of this driver changes: refuses the change when a running
-- campaign the driver is enrolled in has onFleetChange = BLOCK and the driver is inside the day's
-- program window (first window start to last window end).
checkFleetChangeAllowed :: MgEventFlow m r => Id DP.Person -> m ()
checkFleetChangeAllowed driverId = do
  now <- getCurrentTime
  ctxs <- getMgCtxs driverId
  forM_ ctxs $ \ctx ->
    when (insideProgramWindow now ctx) $ do
      mbCampaign <- QMgC.findByPrimaryKey ctx.campaignId
      whenJust mbCampaign $ \campaign ->
        when (campaign.onFleetChange == DMgC.BLOCK && campaign.status == DMgC.PUBLISHED) $
          throwError $ InvalidRequest "Driver is enrolled in a minimum guarantee campaign until the end of today's last window; the fleet cannot be changed before then"

-- | After a fleet association of this driver changed. Inside the program window the campaign's
-- onFleetChange policy decides (DISQUALIFY or CANCEL; BLOCK was enforced before the change). Before
-- the window starts the day is simply cancelled: the payee fixed at opt-in is no longer right.
onFleetChanged :: MgEventFlow m r => Id DP.Person -> m ()
onFleetChanged driverId = do
  now <- getCurrentTime
  withLiveDays driverId $ \ctx enrollment campaign _rules -> do
    let beforeWindows = maybe False (now <) (spanStartOf ctx)
    if beforeWindows || insideProgramWindow now ctx
      then
        let policy = if beforeWindows then DMgC.CANCEL else campaign.onFleetChange
         in disqualifyOrCancelDay now campaign policy ctx enrollment
      else pure ()

-- | Ends a day without a top-up. DISQUALIFY is a violation; CANCEL (and BLOCK reaching here, which
-- only happens if a change slipped past the check) is an admin-style cancellation. The day leaves
-- the Redis entry either way.
disqualifyOrCancelDay :: MgEventFlow m r => UTCTime -> DMgC.MgCampaign -> DMgC.MgFleetChangePolicy -> MgDayCtx -> DMgE.MgEnrollment -> m ()
disqualifyOrCancelDay now campaign policy ctx enrollment = do
  let (status, eventType) = case policy of
        DMgC.DISQUALIFY -> (DMgE.DISQUALIFIED, DMgE.DAY_DISQUALIFIED)
        _ -> (DMgE.CANCELLED_BY_ADMIN, DMgE.DAY_CANCELLED_BY_ADMIN)
      events1 = appendEvent now DMgE.FLEET_CHANGED (Just "fleet association changed") Nothing enrollment.events
      events2 = appendEvent now eventType Nothing (Just 0) events1
  QMgE.updateByPrimaryKey
    enrollment
      { DMgE.status = status,
        DMgE.payoutPercent = Just 0,
        DMgE.events = events2
      }
  unregisterMgCtx enrollment.driverId ctx.enrollmentId
  -- a cancelled day gives its reservation back; a disqualified one keeps it
  when (status == DMgE.CANCELLED_BY_ADMIN) $ releaseBudget campaign
  when (status == DMgE.DISQUALIFIED) $ notifyDriverOfDisqualification enrollment.driverId

spanStartOf :: MgDayCtx -> Maybe UTCTime
spanStartOf ctx = listToMaybe (map (.spanStart) ctx.spans)

insideProgramWindow :: UTCTime -> MgDayCtx -> Bool
insideProgramWindow now ctx = case (spanStartOf ctx, lastMaybe (map (.spanEnd) ctx.spans)) of
  (Just firstStart, Just lastEnd) -> now >= firstStart && now < lastEnd
  _ -> False
