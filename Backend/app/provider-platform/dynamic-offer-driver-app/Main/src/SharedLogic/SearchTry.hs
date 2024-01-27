{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchTry where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, logDebug, throwError)
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

initiateDriverSearchBatch ::
  DM.Merchant ->
  DSR.SearchRequest ->
  DTC.TripCategory ->
  DVeh.Variant ->
  Text ->
  Maybe Money ->
  Text ->
  Flow ()
initiateDriverSearchBatch merchant searchReq tripCategory vehicleVariant estOrQuoteId customerExtraFee messageId = do
  farePolicy <- getFarePolicy searchReq.merchantOperatingCityId tripCategory vehicleVariant searchReq.area
  searchTry <- createNewSearchTry farePolicy searchReq.customerCancellationDues
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleVariant searchTry.tripCategory searchReq.estimatedDistance
  goHomeCfg <- CQGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId
  let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
  (res, isGoHomeBatch) <- sendSearchRequestToDrivers' driverPoolConfig searchReq searchTry merchant driverExtraFeeBounds goHomeCfg
  let inTime = fromIntegral (if isGoHomeBatch then goHomeCfg.goHomeBatchDelay else driverPoolConfig.singleBatchProcessTime)
  case res of
    ReSchedule _ -> do
      maxShards <- asks (.maxShards)
      JC.createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
        SendSearchRequestToDriverJobData
          { searchTryId = searchTry.id,
            estimatedRideDistance = searchReq.estimatedDistance,
            driverExtraFeeBounds = driverExtraFeeBounds
          }
    _ -> return ()
  where
    createNewSearchTry :: DFP.FullFarePolicy -> HighPrecMoney -> Flow DST.SearchTry
    createNewSearchTry farePolicy customerCancellationDues = do
      mbLastSearchTry <- QST.findLastByRequestId searchReq.id
      fareParams <-
        calculateFareParameters
          CalculateFareParametersParams
            { farePolicy = farePolicy,
              distance = fromMaybe 0 searchReq.estimatedDistance, -- TODO: Fix this
              rideTime = searchReq.startTime,
              waitingTime = Nothing,
              actualRideDuration = Nothing,
              avgSpeedOfVehicle = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = customerExtraFee,
              nightShiftCharge = Nothing,
              ..
            }
      let estimatedFare = fareSum fareParams
          pureEstimatedFare = pureFareSum fareParams
      searchTry <- case mbLastSearchTry of
        Nothing -> do
          searchTry <- buildSearchTry merchant.id searchReq.merchantOperatingCityId searchReq.id estOrQuoteId estimatedFare 0 DST.INITIAL tripCategory customerExtraFee searchReq.startTime messageId vehicleVariant
          _ <- QST.create searchTry
          return searchTry
        Just oldSearchTry -> do
          let searchRepeatType = if oldSearchTry.status == DST.ACTIVE then DST.CANCELLED_AND_RETRIED else DST.RETRIED
          unless (pureEstimatedFare == oldSearchTry.baseFare - fromMaybe 0 oldSearchTry.customerExtraFee) $
            throwError SearchTryEstimatedFareChanged
          searchTry <- buildSearchTry merchant.id searchReq.merchantOperatingCityId searchReq.id estOrQuoteId estimatedFare (oldSearchTry.searchRepeatCounter + 1) searchRepeatType tripCategory customerExtraFee searchReq.startTime messageId vehicleVariant
          when (oldSearchTry.status == DST.ACTIVE) $ do
            QST.updateStatus oldSearchTry.id DST.CANCELLED
            void $ QDQ.setInactiveBySTId oldSearchTry.id
          _ <- QST.create searchTry
          return searchTry

      logDebug $
        "search try id=" <> show searchTry.id
          <> "; estimated distance = "
          <> show searchReq.estimatedDistance
          <> "; estimated base fare:"
          <> show estimatedFare
      return searchTry

buildSearchTry ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DSR.SearchRequest ->
  Text ->
  Money ->
  Int ->
  DST.SearchRepeatType ->
  DTC.TripCategory ->
  Maybe Money ->
  UTCTime ->
  Text ->
  DVeh.Variant ->
  m DST.SearchTry
buildSearchTry merchantId merchantOpCityId searchReqId estOrQuoteId baseFare searchRepeatCounter searchRepeatType tripCategory customerExtraFee pickupTime messageId vehicleVariant = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` pickupTime
  pure
    DST.SearchTry
      { id = id_,
        vehicleVariant,
        requestId = searchReqId,
        estimateId = estOrQuoteId,
        merchantId = Just merchantId,
        merchantOperatingCityId = merchantOpCityId,
        messageId = messageId,
        startTime = pickupTime,
        validTill = validTill_,
        status = DST.ACTIVE,
        createdAt = now,
        updatedAt = now,
        ..
      }
