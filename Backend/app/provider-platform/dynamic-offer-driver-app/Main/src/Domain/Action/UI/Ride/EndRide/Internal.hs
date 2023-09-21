{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    putDiffMetric,
    getDistanceBetweenPoints,
    makeDailyDriverLeaderBoardKey,
    safeMod,
    makeWeeklyDriverLeaderBoardKey,
    getCurrentDate,
    getParamsfromZscore,
    makeCachedDailyDriverLeaderBoardKey,
    makeCachedWeeklyDriverLeaderBoardKey,
    mkDriverFeeCalcJobFlagKey,
    getDriverFeeCalcJobFlagKey,
    getPlan,
    getDriverFeeBillNumberKey,
    mkDriverFeeBillNumberKey,
  )
where

-- import Storage.CachedQueries.LeaderBoardConfig as QLeaderConfig

import qualified Data.Map as M
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.FareParameters as DFare
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.LeaderBoardConfig as LConfig
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import EulerHS.Prelude hiding (foldr, id, length, null)
import GHC.Float (double2Int)
import Kernel.External.Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude hiding (whenJust)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow, EsqLocRepDBFlow)
import Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (getCurrentTime)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.DriverLocation as DLoc
import SharedLogic.DriverOnboarding
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import Storage.Queries.DriverPlan (findByDriverId)
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications (sendNotificationToDriver)

endRideTransaction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqLocDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqLocRepDBFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasField "maxShards" r Int,
    EventStreamFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "enableLocationTrackingService" r Bool
  ) =>
  Id DP.Driver ->
  SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  DFare.FareParameters ->
  TransporterConfig ->
  Id Merchant ->
  m ()
endRideTransaction driverId booking ride mbFareParams mbRiderDetailsId newFareParams thresholdConfig merchantId = do
  DLoc.updateOnRide merchantId ride.driverId False
  QRide.updateStatus ride.id Ride.COMPLETED
  QRB.updateStatus booking.id SRB.COMPLETED
  whenJust mbFareParams QFare.create
  QRide.updateAll ride.id ride

  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  if driverInfo.active
    then QDFS.updateStatus ride.driverId DDFS.ACTIVE
    else QDFS.updateStatus ride.driverId DDFS.IDLE
  QDriverStats.updateIdleTime driverId
  QDriverStats.incrementTotalRidesAndTotalDist (cast ride.driverId) (fromMaybe 0 ride.chargeableDistance)

  when (thresholdConfig.subscription) $ do
    maxShards <- asks (.maxShards)
    createDriverFee merchantId driverId ride.fare newFareParams maxShards driverInfo

  triggerRideEndEvent RideEventData {ride = ride{status = Ride.COMPLETED}, personId = cast driverId, merchantId = merchantId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = SRB.COMPLETED}, personId = cast driverId, merchantId = merchantId}

  -- sendReferralFCM ride mbRiderDetailsId
  -- updateLeaderboardZScore merchantId ride mbRiderDetailsId
  updateLeaderboardZScoreAndSendReferralFCM merchantId ride mbRiderDetailsId
  DS.driverScoreEventHandler DST.OnRideCompletion {merchantId = merchantId, driverId = cast driverId, ride = ride}

-- sendReferralFCM ::
--   ( CacheFlow m r,
--     EsqDBFlow m r,
--     Esq.EsqDBReplicaFlow m r,
--     HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
--   ) =>
--   Ride.Ride ->
--   Maybe (Id RD.RiderDetails) ->
--   m ()
-- sendReferralFCM ride mbRiderDetailsId = do
--   mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId
--   minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
--   let shouldUpdateRideComplete =
--         case minTripDistanceForReferralCfg of
--           Just distance -> (metersToHighPrecMeters <$> ride.chargeableDistance) >= Just distance && maybe True (not . (.hasTakenValidRide)) mbRiderDetails
--           Nothing -> True
--   when shouldUpdateRideComplete $
--     fork "REFERRAL_ACTIVATED FCM to Driver" $ do
--       whenJust mbRiderDetails $ \riderDetails -> do
--         QRD.updateHasTakenValidRide riderDetails.id
--         case riderDetails.referredByDriver of
--           Just referredDriverId -> do
--             let referralMessage = "Congratulations!"
--             let referralTitle = "Your referred customer has completed their first Namma Yatri ride"
--             driver <- SQP.findById referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
--             sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver.id driver.deviceToken
--           Nothing -> pure ()

-- updateLeaderboardZScore ::
--   ( CacheFlow m r,
--     EsqDBFlow m r,
--     Esq.EsqDBReplicaFlow m r,
--     HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
--   ) =>
--   Id Merchant ->
--   Ride.Ride ->
--   Maybe (Id RD.RiderDetails) ->
--   m ()
-- updateLeaderboardZScore merchantId ride mbRiderDetailsId = do
--   fork "Updating ZScore for driver" . Hedis.withNonCriticalRedis $ do
--     mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId
--     minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
--     let shouldUpdateRideComplete = case minTripDistanceForReferralCfg of
--                                       Just distance -> (metersToHighPrecMeters <$> ride.chargeableDistance) >= Just distance && maybe True (not . (.hasTakenValidRide)) mbRiderDetails
--                                       Nothing -> True
--     nowUtc <- getCurrentTime
--     let rideDate = getCurrentDate nowUtc
--     driverZscore <- Hedis.zScore (makeDailyDriverLeaderBoardKey merchantId rideDate LConfig.RIDE) $ ride.driverId.getId
--     updateDriverDailyZscore ride.driverId rideDate driverZscore ride.chargeableDistance merchantId LConfig.RIDE shouldUpdateRideComplete
--     let (_, currDayIndex) = sundayStartWeek rideDate
--     let weekStartDate = addDays (fromIntegral (- currDayIndex)) rideDate
--     let weekEndDate = addDays (fromIntegral (6 - currDayIndex)) rideDate
--     driverWeeklyZscore <- Hedis.zScore (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate LConfig.RIDE) $ ride.driverId.getId
--     updateDriverWeeklyZscore ride.driverId rideDate weekStartDate weekEndDate driverWeeklyZscore ride.chargeableDistance merchantId LConfig.RIDE shouldUpdateRideComplete

--     whenJust mbRiderDetails $ \riderDetails -> do
--       case riderDetails.referredByDriver of
--         Just referredDriverId -> do
--           driverReferralZscore <- Hedis.zScore (makeDailyDriverLeaderBoardKey merchantId rideDate LConfig.REFERRAL) $ referredDriverId.getId
--           updateDriverDailyZscore referredDriverId rideDate driverReferralZscore ride.chargeableDistance merchantId LConfig.REFERRAL shouldUpdateRideComplete
--           driveReferralrWeeklyZscore <- Hedis.zScore (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate LConfig.REFERRAL) $ ride.driverId.getId
--           updateDriverWeeklyZscore referredDriverId rideDate weekStartDate weekEndDate driveReferralrWeeklyZscore ride.chargeableDistance merchantId LConfig.REFERRAL shouldUpdateRideComplete
--         Nothing -> pure ()

updateLeaderboardZScoreAndSendReferralFCM ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  Id Merchant ->
  Ride.Ride ->
  Maybe (Id RD.RiderDetails) ->
  m ()
updateLeaderboardZScoreAndSendReferralFCM merchantId ride mbRiderDetailsId = do
  mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> (metersToHighPrecMeters <$> ride.chargeableDistance) >= Just distance && maybe True (not . (.hasTakenValidRide)) mbRiderDetails
          Nothing -> True

  when shouldUpdateRideComplete $
    fork "REFERRAL_ACTIVATED FCM to Driver" $ do
      whenJust mbRiderDetails $ \riderDetails -> do
        QRD.updateHasTakenValidRide riderDetails.id
        case riderDetails.referredByDriver of
          Just referredDriverId -> do
            let referralMessage = "Congratulations!"
            let referralTitle = "Your referred customer has completed their first Namma Yatri ride"
            driver <- SQP.findById referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
            sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver.id driver.deviceToken
          Nothing -> pure ()

  fork "Updating ZScore for driver" . Hedis.withNonCriticalRedis $ do
    nowUtc <- getCurrentTime
    let rideDate = getCurrentDate nowUtc
    driverZscore <- Hedis.zScore (makeDailyDriverLeaderBoardKey merchantId rideDate LConfig.RIDE) $ ride.driverId.getId
    updateDriverDailyZscore ride.driverId rideDate driverZscore ride.chargeableDistance merchantId LConfig.RIDE shouldUpdateRideComplete
    let (_, currDayIndex) = sundayStartWeek rideDate
    let weekStartDate = addDays (fromIntegral (- currDayIndex)) rideDate
    let weekEndDate = addDays (fromIntegral (6 - currDayIndex)) rideDate
    driverWeeklyZscore <- Hedis.zScore (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate LConfig.RIDE) $ ride.driverId.getId
    updateDriverWeeklyZscore ride.driverId rideDate weekStartDate weekEndDate driverWeeklyZscore ride.chargeableDistance merchantId LConfig.RIDE shouldUpdateRideComplete

    whenJust mbRiderDetails $ \riderDetails -> do
      case riderDetails.referredByDriver of
        Just referredDriverId -> do
          driverReferralZscore <- Hedis.zScore (makeDailyDriverLeaderBoardKey merchantId rideDate LConfig.REFERRAL) $ referredDriverId.getId
          updateDriverDailyZscore referredDriverId rideDate driverReferralZscore ride.chargeableDistance merchantId LConfig.REFERRAL shouldUpdateRideComplete
          driveReferralrWeeklyZscore <- Hedis.zScore (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate LConfig.REFERRAL) $ ride.driverId.getId
          updateDriverWeeklyZscore referredDriverId rideDate weekStartDate weekEndDate driveReferralrWeeklyZscore ride.chargeableDistance merchantId LConfig.REFERRAL shouldUpdateRideComplete
        Nothing -> pure ()

updateDriverDailyZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Id DP.Person -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> LConfig.DriverLeaderBoardType -> Bool -> m ()
updateDriverDailyZscore driverId rideDate driverZscore chargeableDistance merchantId driverLeaderBoardType validCustomer = do
  mbdDailyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY merchantId
  whenJust mbdDailyLeaderBoardConfig $ \dailyLeaderBoardConfig -> do
    when dailyLeaderBoardConfig.isEnabled $ do
      (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
      let lbExpiry = dailyLeaderBoardConfig.leaderBoardExpiry - secondsFromTimeOfDay localTime
      let currZscore =
            case driverZscore of
              Nothing -> dailyLeaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 chargeableDistance)
              Just zscore -> do
                let (prevTotalRides, prevTotalDistance, prevTotalValidCustomers, prevTotalReferrals) = getParamsfromZscore zscore dailyLeaderBoardConfig.zScoreBase driverLeaderBoardType
                case driverLeaderBoardType of
                  LConfig.RIDE -> do
                    let currTotalRides = prevTotalRides + 1
                    let currTotalDist = prevTotalDistance + fromMaybe 0 chargeableDistance
                    currTotalRides * dailyLeaderBoardConfig.zScoreBase + getMeters currTotalDist
                  LConfig.REFERRAL -> do
                    let currTotalValidCustomers = prevTotalValidCustomers + (if validCustomer then 1 else 0)
                    let currTotalReferrals = prevTotalReferrals + 1
                    currTotalValidCustomers * dailyLeaderBoardConfig.zScoreBase + currTotalReferrals
      Hedis.zAddExp (makeDailyDriverLeaderBoardKey merchantId rideDate driverLeaderBoardType) driverId.getId (fromIntegral currZscore) lbExpiry.getSeconds
      let limit = dailyLeaderBoardConfig.leaderBoardLengthLimit
      driversListWithScores' <- Hedis.zrevrangeWithscores (makeDailyDriverLeaderBoardKey merchantId rideDate driverLeaderBoardType) 0 (limit -1)
      Hedis.setExp (makeCachedDailyDriverLeaderBoardKey merchantId rideDate driverLeaderBoardType) driversListWithScores' (dailyLeaderBoardConfig.leaderBoardExpiry.getSeconds * dailyLeaderBoardConfig.numberOfSets)

updateDriverWeeklyZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Id DP.Person -> Day -> Day -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> LConfig.DriverLeaderBoardType -> Bool -> m ()
updateDriverWeeklyZscore driverId rideDate weekStartDate weekEndDate driverZscore rideChargeableDistance merchantId driverLeaderBoardType validCustomer = do
  mbWeeklyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY merchantId
  whenJust mbWeeklyLeaderBoardConfig $ \weeklyLeaderBoardConfig -> do
    when weeklyLeaderBoardConfig.isEnabled $ do
      (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
      let (_, currDayIndex) = sundayStartWeek rideDate
      let lbExpiry = weeklyLeaderBoardConfig.leaderBoardExpiry - Seconds ((currDayIndex + 1) * 86400) + (Seconds 86400 - secondsFromTimeOfDay localTime) -- Calculated as (total_Seconds_in_week - Week_Day_Index * 86400 + Seconds_Remaining_In_This_Week
      let currZscore =
            case driverZscore of
              Nothing -> weeklyLeaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 rideChargeableDistance)
              Just zscore -> do
                let (prevTotalRides, prevTotalDistance, prevTotalValidCustomers, prevTotalReferrals) = getParamsfromZscore zscore weeklyLeaderBoardConfig.zScoreBase driverLeaderBoardType
                case driverLeaderBoardType of
                  LConfig.RIDE -> do
                    let currTotalRides = prevTotalRides + 1
                    let currTotalDist = prevTotalDistance + fromMaybe 0 rideChargeableDistance
                    currTotalRides * weeklyLeaderBoardConfig.zScoreBase + getMeters currTotalDist
                  LConfig.REFERRAL -> do
                    let currTotalValidCustomers = prevTotalValidCustomers + (if validCustomer then 1 else 0)
                    let currTotalReferrals = prevTotalReferrals + 1
                    currTotalValidCustomers * weeklyLeaderBoardConfig.zScoreBase + currTotalReferrals

      Hedis.zAddExp (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate driverLeaderBoardType) driverId.getId (fromIntegral currZscore) (lbExpiry.getSeconds)
      let limit = weeklyLeaderBoardConfig.leaderBoardLengthLimit
      driversListWithScores' <- Hedis.zrevrangeWithscores (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate driverLeaderBoardType) 0 (limit -1)
      Hedis.setExp (makeCachedWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate driverLeaderBoardType) driversListWithScores' (weeklyLeaderBoardConfig.leaderBoardExpiry.getSeconds * weeklyLeaderBoardConfig.numberOfSets)

makeCachedDailyDriverLeaderBoardKey :: Id Merchant -> Day -> LConfig.DriverLeaderBoardType -> Text
makeCachedDailyDriverLeaderBoardKey merchantId rideDate leaderBoardType =
  case leaderBoardType of
    LConfig.RIDE -> "DDLBCK:" <> merchantId.getId <> ":" <> show rideDate
    LConfig.REFERRAL -> "DDREFRLBCK:" <> merchantId.getId <> ":" <> show rideDate

makeDailyDriverLeaderBoardKey :: Id Merchant -> Day -> LConfig.DriverLeaderBoardType -> Text
makeDailyDriverLeaderBoardKey merchantId rideDate leaderBoardType =
  case leaderBoardType of
    LConfig.RIDE -> "DDLBK:" <> merchantId.getId <> ":" <> show rideDate
    LConfig.REFERRAL -> "DDREFRLBK:" <> merchantId.getId <> ":" <> show rideDate

makeCachedWeeklyDriverLeaderBoardKey :: Id Merchant -> Day -> Day -> LConfig.DriverLeaderBoardType -> Text
makeCachedWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate leaderBoardType =
  case leaderBoardType of
    LConfig.RIDE -> "DWLBCK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate
    LConfig.REFERRAL -> "DWREFRLBCK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate

makeWeeklyDriverLeaderBoardKey :: Id Merchant -> Day -> Day -> LConfig.DriverLeaderBoardType -> Text
makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate leaderBoardType =
  case leaderBoardType of
    LConfig.RIDE -> "DWLBK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate
    LConfig.REFERRAL -> "DWREFRLBK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate

getParamsfromZscore :: Double -> Int -> LConfig.DriverLeaderBoardType -> (Int, Meters, Int, Int)
getParamsfromZscore dzscore dailyZscoreBase leaderBoardType = do
  let (val1, val2) = quotRem (double2Int dzscore) dailyZscoreBase
  case leaderBoardType of
    LConfig.RIDE -> (val1, Meters val2, 0, 0)
    LConfig.REFERRAL -> (0, Meters 0, val1, val2)

getCurrentDate :: UTCTime -> Day
getCurrentDate time =
  let currentDate = localDay $ utcToLocalTime timeZoneIST time
   in currentDate

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs

getDistanceBetweenPoints ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  LatLong ->
  LatLong ->
  [LatLong] ->
  m Meters
getDistanceBetweenPoints merchantId origin destination interpolatedPoints = do
  -- somehow interpolated points pushed to redis in reversed order, so we need to reverse it back
  let pickedWaypoints = origin :| (pickWaypoints interpolatedPoints <> [destination])
  logTagInfo "endRide" $ "pickedWaypoints: " <> show pickedWaypoints
  routeResponse <-
    Maps.getRoutes merchantId $
      Maps.GetRoutesReq
        { waypoints = pickedWaypoints,
          mode = Just Maps.CAR,
          calcPoints = True
        }
  let mbShortestRouteDistance = (.distance) =<< getRouteInfoWithShortestDuration routeResponse
  -- Next error is impossible, because we never receive empty list from directions api
  mbShortestRouteDistance & fromMaybeM (InvalidRequest "Couldn't calculate route distance")

-- TODO reuse code from rider-app
getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration [] = Nothing
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteResult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteResult
  where
    comparator route1 route2 =
      if route1.duration < route2.duration
        then route1
        else route2

-- for distance api we can't pick more than 10 waypoints
pickWaypoints :: [a] -> [a]
pickWaypoints waypoints = do
  let step = length waypoints `div` 10
  take 10 $ foldr (\(n, waypoint) list -> if n `safeMod` step == 0 then waypoint : list else list) [] $ zip [1 ..] waypoints

safeMod :: Int -> Int -> Int
_ `safeMod` 0 = 0
a `safeMod` b = a `mod` b

createDriverFee ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Id Merchant ->
  Id DP.Driver ->
  Maybe Money ->
  DFare.FareParameters ->
  Int ->
  DI.DriverInformation ->
  m ()
createDriverFee merchantId driverId rideFare newFareParams maxShards driverInfo = do
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
  mbDriverPlan <- findByDriverId (cast driverId)
  unless (freeTrialDaysLeft > 0 || (transporterConfig.isPlanMandatory && isNothing mbDriverPlan)) $ do
    let govtCharges = fromMaybe 0 newFareParams.govtCharges
    let (platformFee, cgst, sgst) = case newFareParams.fareParametersDetails of
          DFare.ProgressiveDetails _ -> (0, 0, 0)
          DFare.SlabDetails fpDetails -> (maybe 0 fromIntegral fpDetails.platformFee, fromMaybe 0 fpDetails.cgst, fromMaybe 0 fpDetails.sgst)
    let totalDriverFee = fromIntegral govtCharges + platformFee + cgst + sgst
    now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    lastDriverFee <- QDF.findLatestFeeByDriverId driverId
    driverFee <- mkDriverFee now merchantId driverId rideFare govtCharges platformFee cgst sgst transporterConfig
    when (totalDriverFee > 0 || isJust mbDriverPlan) $ do
      _ <- case lastDriverFee of
        Just ldFee ->
          if now >= ldFee.startTime && now < ldFee.endTime
            then QDF.updateFee ldFee.id rideFare govtCharges platformFee cgst sgst now True
            else QDF.create driverFee
        Nothing -> QDF.create driverFee
      scheduleJobs transporterConfig driverFee merchantId maxShards now

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, HasField "schedulerSetName" r Text, HasField "schedulerType" r SchedulerType, HasField "jobInfoMap" r (M.Map Text Bool)) => TransporterConfig -> DF.DriverFee -> Id Merchant -> Int -> UTCTime -> m ()
scheduleJobs transporterConfig driverFee merchantId maxShards now = do
  void $
    case transporterConfig.driverFeeCalculationTime of
      Nothing -> do
        isPendingPaymentJobScheduled <- getPendingPaymentCache driverFee.endTime
        let pendingPaymentJobTs = diffUTCTime driverFee.endTime now
        case isPendingPaymentJobScheduled of
          Nothing -> do
            createJobIn @_ @'SendPaymentReminderToDriver pendingPaymentJobTs maxShards $
              SendPaymentReminderToDriverJobData
                { startTime = driverFee.startTime,
                  endTime = driverFee.endTime,
                  timeDiff = transporterConfig.timeDiffFromUtc,
                  merchantId = Just merchantId
                }
            setPendingPaymentCache driverFee.endTime pendingPaymentJobTs
          _ -> pure ()
        let overduePaymentJobTs = diffUTCTime driverFee.payBy now
        isOverduePaymentJobScheduled <- getOverduePaymentCache driverFee.endTime
        case isOverduePaymentJobScheduled of
          Nothing -> do
            createJobIn @_ @'UnsubscribeDriverForPaymentOverdue overduePaymentJobTs maxShards $
              UnsubscribeDriverForPaymentOverdueJobData
                { startTime = driverFee.startTime,
                  timeDiff = transporterConfig.timeDiffFromUtc,
                  merchantId = Just merchantId
                }
            setOverduePaymentCache driverFee.endTime overduePaymentJobTs
          _ -> pure ()
      Just dfCalcTime -> do
        isDfCaclculationJobScheduled <- getDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantId
        let dfCalculationJobTs = diffUTCTime (addUTCTime dfCalcTime driverFee.endTime) now
        case isDfCaclculationJobScheduled of
          ----- marker ---
          Nothing -> do
            createJobIn @_ @'CalculateDriverFees dfCalculationJobTs maxShards $
              CalculateDriverFeesJobData
                { merchantId = merchantId,
                  startTime = driverFee.startTime,
                  endTime = driverFee.endTime
                }
            setDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantId dfCalculationJobTs
            setDriverFeeBillNumberKey merchantId 1 36000
          _ -> pure ()

mkDriverFee ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r
  ) =>
  UTCTime ->
  Id Merchant ->
  Id DP.Driver ->
  Maybe Money ->
  Money ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  TransporterConfig ->
  m DF.DriverFee
mkDriverFee now merchantId driverId rideFare govtCharges platformFee cgst sgst transporterConfig = do
  id <- generateGUID
  let potentialStart = addUTCTime transporterConfig.driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
      startTime = if now >= potentialStart then potentialStart else addUTCTime (-1 * transporterConfig.driverPaymentCycleDuration) potentialStart
      endTime = addUTCTime transporterConfig.driverPaymentCycleDuration startTime
      payBy = if isNothing transporterConfig.driverFeeCalculationTime then addUTCTime transporterConfig.driverPaymentCycleBuffer endTime else addUTCTime (transporterConfig.driverAutoPayNotificationTime + transporterConfig.driverAutoPayExecutionTime) endTime
      platformFee_ = if isNothing transporterConfig.driverFeeCalculationTime then DF.PlatformFee platformFee cgst sgst else DF.PlatformFee 0 0 0
      govtCharges_ = if isNothing transporterConfig.driverFeeCalculationTime then govtCharges else 0
  mbDriverPlan <- findByDriverId (cast driverId) -- what if its changed? needed inside lock?
  plan <- getPlan mbDriverPlan merchantId
  return $
    DF.DriverFee
      { status = DF.ONGOING,
        collectedBy = Nothing,
        numRides = 1,
        createdAt = now,
        updatedAt = now,
        platformFee = platformFee_,
        totalEarnings = fromMaybe 0 rideFare,
        feeType = case plan of
          Nothing -> DF.RECURRING_INVOICE
          Just plan_ -> if plan_.paymentMode == MANUAL then DF.RECURRING_INVOICE else DF.RECURRING_EXECUTION_INVOICE,
        govtCharges = govtCharges_,
        offerId = Nothing,
        planOfferTitle = Nothing,
        autopayPaymentStage = Nothing,
        stageUpdatedAt = Nothing,
        billNumber = Nothing,
        feeWithoutDiscount = Nothing, -- Only for NY rn
        ..
      }

getPlan :: (MonadFlow m, CacheFlow m r) => Maybe DriverPlan -> Id Merchant -> m (Maybe Plan)
getPlan mbDriverPlan merchantId = do
  case mbDriverPlan of
    Just dp -> CQP.findByIdAndPaymentMode dp.planId dp.planType
    Nothing -> do
      plans <- CQP.findByMerchantIdAndType merchantId DEFAULT
      case plans of
        [] -> pure Nothing
        [pl] -> pure (Just pl)
        _ -> throwError $ InternalError "Multiple default plans found"

mkPendingPaymentProcessingKey :: UTCTime -> Text
mkPendingPaymentProcessingKey timestamp = "DriverPendingPaymentProcessing:Timestamp:" <> show timestamp

getPendingPaymentCache :: CacheFlow m r => UTCTime -> m (Maybe Bool)
getPendingPaymentCache endTime = Hedis.get (mkPendingPaymentProcessingKey endTime)

setPendingPaymentCache :: CacheFlow m r => UTCTime -> NominalDiffTime -> m ()
setPendingPaymentCache endTime expTime = Hedis.setExp (mkPendingPaymentProcessingKey endTime) False (round expTime)

mkOverduePaymentProcessingKey :: UTCTime -> Text
mkOverduePaymentProcessingKey timestamp = "DriverOverduePaymentProcessing:Timestamp:" <> show timestamp

getOverduePaymentCache :: CacheFlow m r => UTCTime -> m (Maybe Bool)
getOverduePaymentCache endTime = Hedis.get (mkOverduePaymentProcessingKey endTime)

setOverduePaymentCache :: CacheFlow m r => UTCTime -> NominalDiffTime -> m ()
setOverduePaymentCache endTime expTime = Hedis.setExp (mkOverduePaymentProcessingKey endTime) False (round expTime)

getDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id Merchant -> m (Maybe Bool)
getDriverFeeCalcJobCache startTime endTime merchantId = Hedis.get (mkDriverFeeCalcJobCacheKey startTime endTime merchantId)

mkDriverFeeCalcJobCacheKey :: UTCTime -> UTCTime -> Id Merchant -> Text
mkDriverFeeCalcJobCacheKey startTime endTime merchantId = "DriverFeeCalculation:MerchantId:" <> merchantId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime

mkDriverFeeCalcJobFlagKey :: UTCTime -> UTCTime -> Id Merchant -> Text
mkDriverFeeCalcJobFlagKey startTime endTime merchantId = "DriverFeeCalculationFlag:MerchantId:" <> merchantId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime

getDriverFeeCalcJobFlagKey :: CacheFlow m r => UTCTime -> UTCTime -> Id Merchant -> m (Maybe Bool)
getDriverFeeCalcJobFlagKey startTime endTime merchantId = Hedis.get (mkDriverFeeCalcJobFlagKey startTime endTime merchantId)

setDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id Merchant -> NominalDiffTime -> m ()
setDriverFeeCalcJobCache startTime endTime merchantId expTime = do
  Hedis.setExp (mkDriverFeeCalcJobFlagKey startTime endTime merchantId) True (round $ expTime + 86399)
  Hedis.setExp (mkDriverFeeCalcJobCacheKey startTime endTime merchantId) False (round expTime)

mkDriverFeeBillNumberKey :: Id Merchant -> Text
mkDriverFeeBillNumberKey merchantId = "DriverFeeCalulation:BillNumber:Counter" <> merchantId.getId

getDriverFeeBillNumberKey :: CacheFlow m r => Id Merchant -> m (Maybe Int)
getDriverFeeBillNumberKey merchantId = Hedis.get (mkDriverFeeBillNumberKey merchantId)

setDriverFeeBillNumberKey :: CacheFlow m r => Id Merchant -> Int -> NominalDiffTime -> m ()
setDriverFeeBillNumberKey merchantId count expTime = Hedis.setExp (mkDriverFeeBillNumberKey merchantId) count (round expTime)
