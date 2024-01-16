{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Domain.Action.Beckn.Select
  ( DSelectReq (..),
    validateRequest,
    handler,
  )
where

-- import qualified Client.Main as CM
-- import Data.Aeson as DA
-- import Data.HashMap.Strict as HashMap
import Data.Text as Text
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, fromMaybeM, logDebug, throwError)
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    estimateId :: Id DEst.Estimate,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupTime :: UTCTime,
    autoAssignEnabled :: Bool,
    customerExtraFee :: Maybe Money
  }

handler :: DM.Merchant -> DSelectReq -> DEst.Estimate -> Flow ()
handler merchant sReq estimate = do
  now <- getCurrentTime
  searchReq <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
  QDQ.setInactiveAllDQByEstId sReq.estimateId now
  farePolicy <- getFarePolicy searchReq.merchantOperatingCityId estimate.vehicleVariant searchReq.area
  -- farePolicyCond <- liftIO $ CM.hashMapToString $ HashMap.fromList [(pack "merchantOperatingCityId", DA.String (Text.pack ("NAMMA_YATRI"))), (pack "tripDistance", DA.String (Text.pack ("500")))]
  -- contextValue <- liftIO $ CM.evalCtx "test" farePolicyCond
  -- value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
  -- logDebug $ "contextValueEvaluated: " <> show value
  searchTry <- createNewSearchTry farePolicy searchReq searchReq.customerCancellationDues
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId (Just searchTry.vehicleVariant) searchReq.estimatedDistance
  goHomeCfg <- CQGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId
  let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance searchReq.estimatedDistance <$> farePolicy.driverExtraFeeBounds
  (res, isGoHomeBatch) <- sendSearchRequestToDrivers' driverPoolConfig searchReq searchTry merchant driverExtraFeeBounds goHomeCfg
  let inTime = fromIntegral (if isGoHomeBatch then goHomeCfg.goHomeBatchDelay else driverPoolConfig.singleBatchProcessTime)
  case res of
    ReSchedule _ -> do
      maxShards <- asks (.maxShards)
      when sReq.autoAssignEnabled $ QSR.updateAutoAssign searchReq.id sReq.autoAssignEnabled
      JC.createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
        SendSearchRequestToDriverJobData
          { searchTryId = searchTry.id,
            estimatedRideDistance = searchReq.estimatedDistance,
            driverExtraFeeBounds = driverExtraFeeBounds
          }
    _ -> return ()
  where
    createNewSearchTry :: DFP.FullFarePolicy -> DSR.SearchRequest -> HighPrecMoney -> Flow DST.SearchTry
    createNewSearchTry farePolicy searchReq customerCancellationDues = do
      mbLastSearchTry <- QST.findLastByRequestId searchReq.id
      fareParams <-
        calculateFareParameters
          CalculateFareParametersParams
            { farePolicy = farePolicy,
              distance = searchReq.estimatedDistance,
              rideTime = sReq.pickupTime,
              waitingTime = Nothing,
              actualRideDuration = Nothing,
              avgSpeedOfVehicle = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = sReq.customerExtraFee,
              nightShiftCharge = Nothing,
              ..
            }
      let estimatedFare = fareSum fareParams
          pureEstimatedFare = pureFareSum fareParams
      searchTry <- case mbLastSearchTry of
        Nothing -> do
          searchTry <- buildSearchTry merchant.id searchReq.merchantOperatingCityId searchReq.id estimate sReq estimatedFare 0 DST.INITIAL
          _ <- QST.create searchTry
          return searchTry
        Just oldSearchTry -> do
          let searchRepeatType = if oldSearchTry.status == DST.ACTIVE then DST.CANCELLED_AND_RETRIED else DST.RETRIED
          unless (pureEstimatedFare == oldSearchTry.baseFare - fromMaybe 0 oldSearchTry.customerExtraFee) $
            throwError SearchTryEstimatedFareChanged
          searchTry <- buildSearchTry merchant.id searchReq.merchantOperatingCityId searchReq.id estimate sReq estimatedFare (oldSearchTry.searchRepeatCounter + 1) searchRepeatType
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
  DEst.Estimate ->
  DSelectReq ->
  Money ->
  Int ->
  DST.SearchRepeatType ->
  m DST.SearchTry
buildSearchTry merchantId merchantOpCityId searchReqId estimate sReq baseFare searchRepeatCounter searchRepeatType = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` sReq.pickupTime
      customerExtraFee = sReq.customerExtraFee
  pure
    DST.SearchTry
      { id = id_,
        requestId = searchReqId,
        estimateId = estimate.id,
        merchantId = Just merchantId,
        merchantOperatingCityId = merchantOpCityId,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        vehicleVariant = estimate.vehicleVariant,
        status = DST.ACTIVE,
        createdAt = now,
        updatedAt = now,
        ..
      }

validateRequest :: Id DM.Merchant -> DSelectReq -> Flow (DM.Merchant, DEst.Estimate)
validateRequest merchantId sReq = do
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  estimate <- QEst.findById sReq.estimateId >>= fromMaybeM (EstimateDoesNotExist sReq.estimateId.getId)
  return (merchant, estimate)
