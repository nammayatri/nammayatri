{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverScore
  ( driverScoreEventHandler,
  )
where

import qualified Domain.Types.DriverStats as DS
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequestForDriver as SRD
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common (Forkable (fork), fromMaybeM, getCurrentTime, highPrecMetersToMeters, logDebug, logInfo)
import qualified Lib.DriverScore.Types as DST
import qualified SharedLogic.DriverPool as DP
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CTCQ
import qualified Storage.Queries.BookingCancellationReason as BCRQ
import qualified Storage.Queries.DriverStats as DSQ
import qualified Storage.Queries.Ride as RQ
import Tools.Error

driverScoreEventHandler :: (Redis.HedisFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DST.DriverRideRequeset -> m ()
driverScoreEventHandler payload = fork "DRIVER_SCORE_EVENT_HANDLER" do
  logDebug $ "driverScoreEventHandler with payload: " <> show payload
  eventPayloadHandler payload

eventPayloadHandler :: (Redis.HedisFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DST.DriverRideRequeset -> m ()
eventPayloadHandler DST.OnDriverAcceptingSearchRequest {..} = do
  DP.removeSearchReqIdFromMap merchantId driverId searchTryId
  case response of
    SRD.Accept -> do
      DP.incrementQuoteAcceptedCount merchantId driverId
      forM_ restDriverIds $ \restDriverId -> do
        DP.decrementTotalQuotesCount merchantId (cast restDriverId) searchTryId
        DP.removeSearchReqIdFromMap merchantId restDriverId searchTryId
    SRD.Reject -> pure ()
    SRD.Pulled -> pure ()
eventPayloadHandler DST.OnNewRideAssigned {..} = do
  mbDriverStats <- Esq.runInReplica $ DSQ.findById (cast driverId)
  void $ case mbDriverStats of
    Just driverStats -> incrementOrSetTotaRides driverId driverStats
    Nothing -> createDriverStat driverId
  DP.incrementTotalRidesCount merchantId driverId
eventPayloadHandler DST.OnNewSearchRequestForDrivers {..} =
  forM_ driverPool $ \dPoolRes -> DP.incrementTotalQuotesCount searchReq.providerId (cast dPoolRes.driverPoolResult.driverId) searchReq validTill batchProcessTime
eventPayloadHandler DST.OnDriverCancellation {..} = do
  merchantConfig <- CTCQ.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  mbDriverStats <- Esq.runInReplica $ DSQ.findById (cast driverId)
  driverStats <-
    case mbDriverStats of
      Just driverStats ->
        case driverStats.totalRidesAssigned of
          Nothing -> incrementOrSetTotaRides driverId driverStats
          Just _ -> do
            cancelledCount <-
              case driverStats.ridesCancelled of
                Nothing -> Esq.runInReplica $ BCRQ.findAllCancelledByDriverId driverId
                Just cancelledCount -> pure $ cancelledCount + 1
            Esq.runNoTransaction $ DSQ.setCancelledRidesCount (cast driverId) cancelledCount
            pure $ driverStats {DS.ridesCancelled = Just cancelledCount}
      Nothing -> createDriverStat driverId
  cancellationRateExcedded <- overallCancellationRate driverStats merchantConfig
  when (driverStats.totalRidesAssigned > merchantConfig.minRidesToUnlist && cancellationRateExcedded) $ do
    logInfo $ "Blocking Driver: " <> driverId.getId
    CDI.updateBlockedState (cast driverId) True
  DP.incrementCancellationCount merchantId driverId
  where
    overallCancellationRate driverStats merchantConfig = do
      let rate = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int)
          threshold = fromMaybe 65 $ merchantConfig.thresholdCancellationPercentageToUnlist
      logDebug $ "cancellationRate" <> show rate
      pure $ rate > threshold
    nonZero Nothing = 1
    nonZero (Just a)
      | a <= 0 = 1
      | otherwise = a

createDriverStat :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> m DS.DriverStats
createDriverStat driverId = do
  now <- getCurrentTime
  allRides <- Esq.runInReplica $ RQ.findAllRidesByDriverId driverId
  let completedRides = filter ((== DR.COMPLETED) . (.status)) allRides
  cancelledRidesCount <- Esq.runInReplica $ BCRQ.findAllCancelledByDriverId driverId
  let driverStat =
        DS.DriverStats
          { driverId = cast driverId,
            idleSince = now,
            totalRides = length completedRides,
            totalDistance = highPrecMetersToMeters . sum $ map (.traveledDistance) allRides,
            ridesCancelled = Just cancelledRidesCount,
            totalRidesAssigned = Just $ length allRides
          }
  Esq.runNoTransaction $ DSQ.create driverStat
  pure driverStat

incrementOrSetTotaRides :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> DS.DriverStats -> m DS.DriverStats
incrementOrSetTotaRides driverId driverStats = do
  incrementTotaRidesBy <-
    maybe
      ( do
          allRides <- Esq.runInReplica $ RQ.findAllRidesByDriverId driverId
          pure $ length allRides
      )
      (\_ -> pure 1)
      driverStats.totalRidesAssigned
  Esq.runNoTransaction $ DSQ.incrementTotalRidesAssigned (cast driverId) incrementTotaRidesBy
  pure $ driverStats {DS.totalRidesAssigned = Just $ fromMaybe 0 driverStats.totalRidesAssigned + incrementTotaRidesBy}
