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
    getRidesAndDistancefromZscore,
    makeCachedDailyDriverLeaderBoardKey,
    makeCachedWeeklyDriverLeaderBoardKey,
    mkDriverFeeCalcJobFlagKey,
    getDriverFeeCalcJobFlagKey,
    getPlan,
    getDriverFeeBillNumberKey,
    mkDriverFeeBillNumberKey,
  )
where

import qualified Data.Map as M
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CancellationCharges as DCC
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.FareParameters as DFare
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.LeaderBoardConfig as LConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude hiding (foldr, id, length, null)
import GHC.Float (double2Int)
import Kernel.External.Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude hiding (whenJust)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (getCurrentTime)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.DriverOnboarding
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CancellationCharges as QCC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import Storage.Queries.DriverPlan (findByDriverId)
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications
import qualified Tools.PaymentNudge as PaymentNudge

endRideTransaction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasField "maxShards" r Int,
    EventStreamFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Id DP.Driver ->
  SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  DFare.FareParameters ->
  TransporterConfig ->
  m ()
endRideTransaction driverId booking ride mbFareParams mbRiderDetailsId newFareParams thresholdConfig = do
  QDI.updateOnRide (cast ride.driverId) False
  QRide.updateStatus ride.id Ride.COMPLETED
  QRB.updateStatus booking.id SRB.COMPLETED
  whenJust mbFareParams QFare.create
  QRide.updateAll ride.id ride
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  QDriverStats.updateIdleTime driverId
  QDriverStats.incrementTotalRidesAndTotalDist (cast ride.driverId) (fromMaybe 0 ride.chargeableDistance)

  clearCachedFarePolicyByEstOrQuoteId booking.quoteId

  when (thresholdConfig.subscription) $ do
    maxShards <- asks (.maxShards)
    createDriverFee booking.providerId booking.merchantOperatingCityId driverId ride.fare newFareParams maxShards driverInfo booking

  triggerRideEndEvent RideEventData {ride = ride{status = Ride.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = SRB.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}

  mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId

  sendReferralFCM ride booking.providerId mbRiderDetails booking.merchantOperatingCityId
  updateLeaderboardZScore booking.providerId booking.merchantOperatingCityId ride
  DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnRideCompletion {merchantId = booking.providerId, driverId = cast driverId, ride = ride}

  when (thresholdConfig.canAddCancellationFee && newFareParams.customerCancellationDues > 0) $ do
    case mbRiderDetails of
      Just riderDetails -> do
        id <- generateGUID
        let cancellationCharges =
              DCC.CancellationCharges
                { driverId = cast driverId,
                  rideId = Just ride.id,
                  cancellationCharges = newFareParams.customerCancellationDues,
                  ..
                }
        calDisputeChances <-
          if thresholdConfig.cancellationFee == 0
            then do
              logWarning "Unable to calculate dispute chances used"
              return 0
            else do
              return $ round $ newFareParams.customerCancellationDues / thresholdConfig.cancellationFee
        QRD.updateDisputeChancesUsedAndCancellationDues riderDetails.id (max 0 (riderDetails.disputeChancesUsed - calDisputeChances)) 0 >> QCC.create cancellationCharges
      _ -> logWarning $ "Unable to update customer cancellation dues as RiderDetailsId is NULL with rideId " <> ride.id.getId

sendReferralFCM ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  Ride.Ride ->
  Id Merchant ->
  Maybe RD.RiderDetails ->
  Id DMOC.MerchantOperatingCity ->
  m ()
sendReferralFCM ride merchantId mbRiderDetails merchantOpCityId = do
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
            sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver.id driver.deviceToken
            logDebug "Driver Referral Coin Event"
            fork "DriverToCustomerReferralCoin Event : " $ DC.driverCoinsEvent driver.id merchantId merchantOpCityId (DCT.DriverToCustomerReferral ride.chargeableDistance)
          Nothing -> pure ()

updateLeaderboardZScore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> Ride.Ride -> m ()
updateLeaderboardZScore merchantId merchantOpCityId ride = do
  fork "Updating ZScore for driver" . Hedis.withNonCriticalRedis $ do
    nowUtc <- getCurrentTime
    let rideDate = getCurrentDate nowUtc
    driverZscore <- Hedis.zScore (makeDailyDriverLeaderBoardKey merchantId rideDate) $ ride.driverId.getId
    updateDriverDailyZscore ride rideDate driverZscore ride.chargeableDistance merchantId merchantOpCityId
    let (_, currDayIndex) = sundayStartWeek rideDate
    let weekStartDate = addDays (fromIntegral (- currDayIndex)) rideDate
    let weekEndDate = addDays (fromIntegral (6 - currDayIndex)) rideDate
    driverWeeklyZscore <- Hedis.zScore (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) $ ride.driverId.getId
    updateDriverWeeklyZscore ride rideDate weekStartDate weekEndDate driverWeeklyZscore ride.chargeableDistance merchantId merchantOpCityId

updateDriverDailyZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Ride.Ride -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> Id DMOC.MerchantOperatingCity -> m ()
updateDriverDailyZscore ride rideDate driverZscore chargeableDistance merchantId merchantOpCityId = do
  mbdDailyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY merchantOpCityId
  whenJust mbdDailyLeaderBoardConfig $ \dailyLeaderBoardConfig -> do
    when dailyLeaderBoardConfig.isEnabled $ do
      (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
      let lbExpiry = dailyLeaderBoardConfig.leaderBoardExpiry - secondsFromTimeOfDay localTime
      let currZscore =
            case driverZscore of
              Nothing -> dailyLeaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 chargeableDistance)
              Just zscore -> do
                let (prevTotalRides, prevTotalDistance) = getRidesAndDistancefromZscore zscore dailyLeaderBoardConfig.zScoreBase
                let currTotalRides = prevTotalRides + 1
                let currTotalDist = prevTotalDistance + fromMaybe 0 chargeableDistance
                currTotalRides * dailyLeaderBoardConfig.zScoreBase + getMeters currTotalDist
      Hedis.zAddExp (makeDailyDriverLeaderBoardKey merchantId rideDate) ride.driverId.getId (fromIntegral currZscore) lbExpiry.getSeconds
      let limit = dailyLeaderBoardConfig.leaderBoardLengthLimit
      driversListWithScores' <- Hedis.zrevrangeWithscores (makeDailyDriverLeaderBoardKey merchantId rideDate) 0 (limit -1)
      Hedis.setExp (makeCachedDailyDriverLeaderBoardKey merchantId rideDate) driversListWithScores' (dailyLeaderBoardConfig.leaderBoardExpiry.getSeconds * dailyLeaderBoardConfig.numberOfSets)

updateDriverWeeklyZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Ride.Ride -> Day -> Day -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> Id DMOC.MerchantOperatingCity -> m ()
updateDriverWeeklyZscore ride rideDate weekStartDate weekEndDate driverZscore rideChargeableDistance merchantId merchantOpCityId = do
  mbWeeklyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY merchantOpCityId
  whenJust mbWeeklyLeaderBoardConfig $ \weeklyLeaderBoardConfig -> do
    when weeklyLeaderBoardConfig.isEnabled $ do
      (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
      let (_, currDayIndex) = sundayStartWeek rideDate
      let lbExpiry = weeklyLeaderBoardConfig.leaderBoardExpiry - Seconds ((currDayIndex + 1) * 86400) + (Seconds 86400 - secondsFromTimeOfDay localTime) -- Calculated as (total_Seconds_in_week - Week_Day_Index * 86400 + Seconds_Remaining_In_This_Week
      let currZscore =
            case driverZscore of
              Nothing -> weeklyLeaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 rideChargeableDistance)
              Just zscore -> do
                let (prevTotalRides, prevTotalDistance) = getRidesAndDistancefromZscore zscore weeklyLeaderBoardConfig.zScoreBase
                let currTotalRides = prevTotalRides + 1
                let currTotalDist = prevTotalDistance + fromMaybe 0 rideChargeableDistance
                currTotalRides * weeklyLeaderBoardConfig.zScoreBase + getMeters currTotalDist
      Hedis.zAddExp (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) ride.driverId.getId (fromIntegral currZscore) (lbExpiry.getSeconds)
      let limit = weeklyLeaderBoardConfig.leaderBoardLengthLimit
      driversListWithScores' <- Hedis.zrevrangeWithscores (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) 0 (limit -1)
      Hedis.setExp (makeCachedWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) driversListWithScores' (weeklyLeaderBoardConfig.leaderBoardExpiry.getSeconds * weeklyLeaderBoardConfig.numberOfSets)

makeCachedDailyDriverLeaderBoardKey :: Id Merchant -> Day -> Text
makeCachedDailyDriverLeaderBoardKey merchantId rideDate = "DDLBCK:" <> merchantId.getId <> ":" <> show rideDate

makeDailyDriverLeaderBoardKey :: Id Merchant -> Day -> Text
makeDailyDriverLeaderBoardKey merchantId rideDate =
  "DDLBK:" <> merchantId.getId <> ":" <> show rideDate

makeCachedWeeklyDriverLeaderBoardKey :: Id Merchant -> Day -> Day -> Text
makeCachedWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate =
  "DWLBCK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate

makeWeeklyDriverLeaderBoardKey :: Id Merchant -> Day -> Day -> Text
makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate =
  "DWLBK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate

getRidesAndDistancefromZscore :: Double -> Int -> (Int, Meters)
getRidesAndDistancefromZscore dzscore dailyZscoreBase =
  let (totalRides, totalDistance) = quotRem (double2Int dzscore) dailyZscoreBase
   in (totalRides, Meters totalDistance)

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
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DSR.SearchRequest) ->
  LatLong ->
  LatLong ->
  [LatLong] ->
  m Meters
getDistanceBetweenPoints merchantId merchantOpCityId searchRequestId origin destination interpolatedPoints = do
  -- somehow interpolated points pushed to redis in reversed order, so we need to reverse it back
  let pickedWaypoints = origin :| (pickWaypoints interpolatedPoints <> [destination])
  logTagInfo "endRide" $ "pickedWaypoints: " <> show pickedWaypoints
  routeResponse <-
    Maps.getRoutes merchantId merchantOpCityId searchRequestId $
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
    EncFlow m r,
    MonadFlow m,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType
  ) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  Maybe Money ->
  DFare.FareParameters ->
  Int ->
  DI.DriverInformation ->
  SRB.Booking ->
  m ()
createDriverFee merchantId merchantOpCityId driverId rideFare newFareParams maxShards driverInfo booking = do
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
  mbDriverPlan <- getPlanAndPushToDefualtIfEligible transporterConfig freeTrialDaysLeft
  let govtCharges = fromMaybe 0 newFareParams.govtCharges
  let (platformFee, cgst, sgst) = case newFareParams.fareParametersDetails of
        DFare.ProgressiveDetails _ -> (0, 0, 0)
        DFare.SlabDetails fpDetails -> (fromMaybe 0 fpDetails.platformFee, fromMaybe 0 fpDetails.cgst, fromMaybe 0 fpDetails.sgst)
        DFare.RentalDetails _ -> (0, 0, 0)
  let totalDriverFee = fromIntegral govtCharges + platformFee + cgst + sgst
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  lastDriverFee <- QDF.findLatestFeeByDriverId driverId
  driverFee <- mkDriverFee now merchantId driverId rideFare govtCharges platformFee cgst sgst transporterConfig booking
  let toUpdateOrCreateDriverfee = totalDriverFee > 0 || (totalDriverFee <= 0 && transporterConfig.isPlanMandatory && isJust mbDriverPlan)
  when (toUpdateOrCreateDriverfee && isEligibleForCharge transporterConfig freeTrialDaysLeft) $ do
    numRides <- case lastDriverFee of
      Just ldFee ->
        if now >= ldFee.startTime && now < ldFee.endTime
          then do
            QDF.updateFee ldFee.id rideFare govtCharges platformFee cgst sgst now True booking
            return (ldFee.numRides + 1)
          else do
            QDF.create driverFee
            return 1
      Nothing -> do
        QDF.create driverFee
        return 1
    plan <- getPlan mbDriverPlan merchantId
    fork "Sending switch plan nudge" $ PaymentNudge.sendSwitchPlanNudge transporterConfig driverInfo plan mbDriverPlan numRides
    scheduleJobs transporterConfig driverFee merchantId merchantOpCityId maxShards now
  where
    isEligibleForCharge transporterConfig freeTrialDaysLeft =
      if DTC.isRideOtpBooking booking.tripCategory
        then transporterConfig.considerSpecialZoneRideChargesInFreeTrial || freeTrialDaysLeft <= 0
        else freeTrialDaysLeft <= 0

    getPlanAndPushToDefualtIfEligible :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> Int -> m (Maybe DriverPlan)
    getPlanAndPushToDefualtIfEligible transporterConfig freeTrialDaysLeft' = do
      mbDriverPlan' <- findByDriverId (cast driverId)
      let planMandatory = transporterConfig.isPlanMandatory
          chargeSPZRides = transporterConfig.considerSpecialZoneRideChargesInFreeTrial
          isEligibleForDefaultPlanAfterFreeTrial = freeTrialDaysLeft' <= 0 && planMandatory && transporterConfig.allowDefaultPlanAllocation
          isEligibleForDefaultPlanBeforeFreeTrial = freeTrialDaysLeft' > 0 && chargeSPZRides && planMandatory
      if isNothing mbDriverPlan'
        then do
          case (DTC.isRideOtpBooking booking.tripCategory, isEligibleForDefaultPlanBeforeFreeTrial, isEligibleForDefaultPlanAfterFreeTrial) of
            (True, True, _) -> assignDefaultPlan
            (_, _, True) -> assignDefaultPlan
            _ -> return mbDriverPlan'
        else return mbDriverPlan'
    assignDefaultPlan :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m (Maybe DriverPlan)
    assignDefaultPlan = do
      plans <- CQP.findByMerchantIdAndType merchantId DEFAULT
      case plans of
        (plan' : _) -> do
          newDriverPlan <- Plan.mkDriverPlan plan' driverId
          QDPlan.create newDriverPlan
          QDI.updateAutoPayStatusAndPayerVpa (Just DI.PENDING) Nothing (cast driverId)
          return $ Just newDriverPlan
        _ -> return Nothing

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType) => TransporterConfig -> DF.DriverFee -> Id Merchant -> Id MerchantOperatingCity -> Int -> UTCTime -> m ()
scheduleJobs transporterConfig driverFee merchantId merchantOpCityId maxShards now = do
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
                  merchantOperatingCityId = Just merchantOpCityId,
                  startTime = driverFee.startTime,
                  endTime = driverFee.endTime
                }
            setDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantId dfCalculationJobTs
            setDriverFeeBillNumberKey merchantId 1 36000
          _ -> pure ()

mkDriverFee ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
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
  SRB.Booking ->
  m DF.DriverFee
mkDriverFee now merchantId driverId rideFare govtCharges platformFee cgst sgst transporterConfig booking = do
  id <- generateGUID
  let potentialStart = addUTCTime transporterConfig.driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
      startTime = if now >= potentialStart then potentialStart else addUTCTime (-1 * transporterConfig.driverPaymentCycleDuration) potentialStart
      endTime = addUTCTime transporterConfig.driverPaymentCycleDuration startTime
      payBy = if isNothing transporterConfig.driverFeeCalculationTime then addUTCTime transporterConfig.driverPaymentCycleBuffer endTime else addUTCTime (transporterConfig.driverAutoPayNotificationTime + transporterConfig.driverAutoPayExecutionTime) endTime
      platformFee_ = if isNothing transporterConfig.driverFeeCalculationTime then DF.PlatformFee platformFee cgst sgst else DF.PlatformFee 0 0 0
      govtCharges_ = if isNothing transporterConfig.driverFeeCalculationTime then govtCharges else 0
      isPlanMandatory = transporterConfig.isPlanMandatory
      totalFee = platformFee + cgst + sgst
      (specialZoneRideCount, specialZoneAmount) = specialZoneMetricsIntialization totalFee
  mbDriverPlan <- findByDriverId (cast driverId) -- what if its changed? needed inside lock?
  plan <- getPlan mbDriverPlan merchantId
  return $
    DF.DriverFee
      { status = DF.ONGOING,
        collectedBy = Nothing,
        collectedAt = Nothing,
        numRides = 1,
        createdAt = now,
        updatedAt = now,
        platformFee = platformFee_,
        totalEarnings = fromMaybe 0 rideFare,
        feeType = case (plan, isPlanMandatory) of
          (Nothing, _) -> DF.RECURRING_INVOICE
          (Just plan_, True) -> if plan_.paymentMode == MANUAL then DF.RECURRING_INVOICE else DF.RECURRING_EXECUTION_INVOICE
          (Just _, False) -> DF.RECURRING_INVOICE,
        govtCharges = govtCharges_,
        offerId = Nothing,
        planOfferTitle = Nothing,
        autopayPaymentStage = Nothing,
        stageUpdatedAt = Nothing,
        billNumber = Nothing,
        schedulerTryCount = 0,
        feeWithoutDiscount = Nothing, -- Only for NY rn
        overlaySent = False,
        amountPaidByCoin = Nothing,
        planId = Nothing,
        planMode = Nothing,
        notificationRetryCount = 0,
        badDebtDeclarationDate = Nothing,
        badDebtRecoveryDate = Nothing,
        ..
      }
  where
    specialZoneMetricsIntialization totalFee' = if DTC.isRideOtpBooking booking.tripCategory then (1, totalFee') else (0, 0)

getPlan :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe DriverPlan -> Id Merchant -> m (Maybe Plan)
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
  Hedis.setExp (mkDriverFeeCalcJobCacheKey startTime endTime merchantId) False (round $ expTime + 86399)

mkDriverFeeBillNumberKey :: Id Merchant -> Text
mkDriverFeeBillNumberKey merchantId = "DriverFeeCalulation:BillNumber:Counter" <> merchantId.getId

getDriverFeeBillNumberKey :: CacheFlow m r => Id Merchant -> m (Maybe Int)
getDriverFeeBillNumberKey merchantId = Hedis.get (mkDriverFeeBillNumberKey merchantId)

setDriverFeeBillNumberKey :: CacheFlow m r => Id Merchant -> Int -> NominalDiffTime -> m ()
setDriverFeeBillNumberKey merchantId count expTime = Hedis.setExp (mkDriverFeeBillNumberKey merchantId) count (round expTime)
