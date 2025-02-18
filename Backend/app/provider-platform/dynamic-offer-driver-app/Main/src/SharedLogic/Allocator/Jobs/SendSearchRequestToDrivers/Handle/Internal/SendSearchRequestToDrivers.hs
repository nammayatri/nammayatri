{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers
  ( sendSearchRequestToDrivers,
  )
where

import Control.Monad.Extra (anyM)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.DriverPoolConfig
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Location as DLoc
import Domain.Types.Person (Driver)
import Domain.Types.RiderDetails
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.TransporterConfig as DTR
import Kernel.Beam.Functions
import qualified Kernel.External.Maps as EMaps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion (..))
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler.Environment
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import qualified SharedLogic.DriverPool as SDP
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.DriverStats as QDriverStats
import Storage.Queries.RiderDriverCorrelation
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

type LanguageDictionary = M.Map Maps.Language DSR.SearchRequest

sendSearchRequestToDrivers ::
  ( Log m,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    TranslateFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int, "version" ::: DeploymentVersion],
    LT.HasLocationService m r,
    JobCreator r m
  ) =>
  Bool ->
  [SDP.TripQuoteDetail] ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  DriverPoolConfig ->
  [SDP.DriverPoolWithActualDistResult] ->
  [Id Driver] ->
  GoHomeConfig ->
  m ()
sendSearchRequestToDrivers isAllocatorBatch tripQuoteDetails oldSearchReq searchTry driverPoolConfig driverPool prevBatchDrivers goHomeConfig = do
  logInfo $ "Send search requests to driver pool batch-" <> show driverPool

  -- We update few things during 1st batch in searchReq table which is not being passed in above Search request, hence fetch search request again if it is first batch
  -- isAllocatorBatch is false if it is first batch because 1st batch is always triggered from application, not allocator
  mbSearchReq <- if isAllocatorBatch then pure Nothing else QSR.findById oldSearchReq.id
  let searchReq = fromMaybe oldSearchReq mbSearchReq

  bapMetadata <- CQSM.findBySubscriberIdAndDomain (Id searchReq.bapId) Domain.MOBILITY
  validTill <- getSearchRequestValidTill
  batchNumber <- getPoolBatchNum searchTry.id
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty driverPool
  let tripQuoteDetailsHashMap = HashMap.fromList $ (\tqd -> (tqd.vehicleServiceTier, tqd)) <$> tripQuoteDetails
  DS.driverScoreEventHandler
    searchReq.merchantOperatingCityId
    DST.OnNewSearchRequestForDrivers
      { driverPool = driverPool,
        merchantId = searchReq.providerId,
        searchReq = searchReq,
        searchTry = searchTry,
        validTill = validTill,
        batchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      }

  transporterConfig <- SCTC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId))) >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq tripQuoteDetailsHashMap batchNumber validTill transporterConfig searchReq.riderId) driverPool
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  whenM (anyM (\driverId -> CQDGR.getDriverGoHomeRequestInfo driverId searchReq.merchantOperatingCityId (Just goHomeConfig) <&> isNothing . (.status)) prevBatchDrivers) $ QSRD.setInactiveBySTId searchTry.id -- inactive previous request by drivers so that they can make new offers.
  _ <- QSRD.createMany searchRequestsForDrivers

  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    let needTranslation = language `elem` transporterConfig.languagesToBeTranslated
    let translatedSearchReq =
          if needTranslation
            then fromMaybe searchReq $ M.lookup language languageDictionary
            else searchReq
    isValueAddNP <- CQVAN.isValueAddNP searchReq.bapId
    let useSilentFCMForForwardBatch = transporterConfig.useSilentFCMForForwardBatch
    tripQuoteDetail <- HashMap.lookup dPoolRes.driverPoolResult.serviceTier tripQuoteDetailsHashMap & fromMaybeM (VehicleServiceTierNotFound $ show dPoolRes.driverPoolResult.serviceTier)
    let entityData = USRD.makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq searchTry bapMetadata dPoolRes.intelligentScores.rideRequestPopupDelayDuration dPoolRes.specialZoneExtraTip dPoolRes.keepHiddenForSeconds tripQuoteDetail.vehicleServiceTier needTranslation isValueAddNP useSilentFCMForForwardBatch tripQuoteDetail.driverPickUpCharge tripQuoteDetail.driverParkingCharge
    -- Notify.notifyOnNewSearchRequestAvailable searchReq.merchantOperatingCityId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
    notificationData <- Notify.buildSendSearchRequestNotificationData searchTry.merchantOperatingCityId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData EmptyDynamicParam (Just searchTry.tripCategory)
    let fallBackCity = Notify.getNewMerchantOpCityId sReqFD.clientSdkVersion sReqFD.merchantOperatingCityId
    Notify.sendSearchRequestToDriverNotification searchReq.providerId fallBackCity sReqFD.driverId notificationData
  where
    getBaseFare ::
      ( MonadFlow m,
        Redis.HedisFlow m r,
        HasFlowEnv m r '["version" ::: DeploymentVersion],
        EsqDBFlow m r,
        Esq.EsqDBReplicaFlow m r,
        CacheFlow m r
      ) =>
      DSR.SearchRequest ->
      DFP.FullFarePolicy ->
      Maybe Months ->
      SDP.TripQuoteDetail ->
      DTR.TransporterConfig ->
      m HighPrecMoney
    getBaseFare searchReq farePolicy vehicleAge tripQuoteDetail transporterConfig = do
      fareParams <-
        Fare.calculateFareParameters
          Fare.CalculateFareParametersParams
            { farePolicy = farePolicy,
              actualDistance = searchReq.estimatedDistance,
              estimatedDistance = searchReq.estimatedDistance,
              rideTime = searchReq.startTime,
              returnTime = searchReq.returnTime,
              roundTrip = fromMaybe False searchReq.roundTrip,
              waitingTime = Nothing,
              stopWaitingTimes = [],
              actualRideDuration = Nothing,
              noOfStops = length searchReq.stops,
              estimatedRideDuration = searchReq.estimatedDuration,
              estimatedCongestionCharge = Nothing,
              avgSpeedOfVehicle = transporterConfig.avgSpeedOfVehicle,
              driverSelectedFare = Nothing,
              customerExtraFee = Nothing,
              nightShiftCharge = Nothing,
              customerCancellationDues = Nothing,
              nightShiftOverlapChecking = DTC.isFixedNightCharge tripQuoteDetail.tripCategory,
              timeDiffFromUtc = Just transporterConfig.timeDiffFromUtc,
              tollCharges = Nothing,
              vehicleAge = vehicleAge,
              currency = searchReq.currency,
              distanceUnit = searchReq.distanceUnit,
              merchantOperatingCityId = Just searchReq.merchantOperatingCityId
            }
      pure $ Fare.fareSum fareParams

    getSearchRequestValidTill = do
      now <- getCurrentTime
      let singleBatchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      return $ singleBatchProcessTime `addUTCTime` now
    buildSearchRequestForDriver ::
      ( MonadFlow m,
        Redis.HedisFlow m r,
        HasFlowEnv m r '["version" ::: DeploymentVersion],
        EsqDBFlow m r,
        Esq.EsqDBReplicaFlow m r,
        CacheFlow m r
      ) =>
      DSR.SearchRequest ->
      HashMap.HashMap DVST.ServiceTierType SDP.TripQuoteDetail ->
      Int ->
      UTCTime ->
      DTR.TransporterConfig ->
      Maybe (Id RiderDetails) ->
      SDP.DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver searchReq tripQuoteDetailsHashMap batchNumber defaultValidTill transporterConfig riderId dpwRes = do
      let currency = searchTry.currency
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      driverStats <- runInReplica $ QDriverStats.findById dpRes.driverId
      tripQuoteDetail <- HashMap.lookup dpRes.serviceTier tripQuoteDetailsHashMap & fromMaybeM (VehicleServiceTierNotFound $ show dpRes.serviceTier)
      parallelSearchRequestCount <- Just <$> SDP.getValidSearchRequestCount searchReq.providerId dpRes.driverId now
      baseFare <- case tripQuoteDetail.tripCategory of
        DTC.Ambulance _ -> do
          farePolicy <- getFarePolicyByEstOrQuoteId (Just $ EMaps.getCoordinates searchReq.fromLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration searchReq.merchantOperatingCityId tripQuoteDetail.tripCategory dpRes.serviceTier searchReq.area searchTry.estimateId Nothing Nothing searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId)))
          getBaseFare searchReq farePolicy dpRes.vehicleAge tripQuoteDetail transporterConfig
        _ -> pure tripQuoteDetail.baseFare
      deploymentVersion <- asks (.version)
      isFavourite <- maybe (pure Nothing) (\riderid -> findByRiderIdAndDriverId riderid (cast dpRes.driverId) <&> fmap (.favourite)) riderId
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                requestId = searchReq.id,
                searchTryId = searchTry.id,
                estimateId = Just tripQuoteDetail.estimateOrQuoteId,
                startTime = searchTry.startTime,
                merchantId = Just searchReq.providerId,
                fromLocGeohash = searchReq.fromLocGeohash,
                vehicleAge = dpRes.vehicleAge,
                merchantOperatingCityId = searchReq.merchantOperatingCityId,
                searchRequestValidTill = if dpwRes.pickupZone then addUTCTime (fromIntegral dpwRes.keepHiddenForSeconds) defaultValidTill else defaultValidTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                vehicleServiceTier = tripQuoteDetail.vehicleServiceTier,
                vehicleServiceTierName = Just tripQuoteDetail.vehicleServiceTierName,
                airConditioned = dpRes.isAirConditioned,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                createdAt = now,
                updatedAt = Just now,
                response = Nothing,
                driverMinExtraFee = tripQuoteDetail.driverMinFee,
                driverMaxExtraFee = tripQuoteDetail.driverMaxFee,
                driverStepFee = tripQuoteDetail.driverStepFee,
                driverDefaultStepFee = tripQuoteDetail.driverDefaultStepFee,
                rideRequestPopupDelayDuration = dpwRes.intelligentScores.rideRequestPopupDelayDuration,
                baseFare = Just baseFare,
                currency,
                distanceUnit = searchReq.distanceUnit,
                isPartOfIntelligentPool = dpwRes.isPartOfIntelligentPool,
                acceptanceRatio = dpwRes.intelligentScores.acceptanceRatio,
                cancellationRatio = dpwRes.intelligentScores.cancellationRatio,
                driverAvailableTime = dpwRes.intelligentScores.availableTime,
                driverSpeed = dpwRes.intelligentScores.driverSpeed,
                keepHiddenForSeconds = dpwRes.keepHiddenForSeconds,
                pickupZone = dpwRes.pickupZone,
                mode = dpRes.mode,
                goHomeRequestId = dpwRes.goHomeReqId,
                rideFrequencyScore = dpwRes.intelligentScores.rideFrequency,
                customerCancellationDues = fromMaybe 0 searchReq.customerCancellationDues,
                clientSdkVersion = dpwRes.driverPoolResult.clientSdkVersion,
                clientBundleVersion = dpwRes.driverPoolResult.clientBundleVersion,
                clientConfigVersion = dpwRes.driverPoolResult.clientConfigVersion,
                clientDevice = dpwRes.driverPoolResult.clientDevice,
                backendConfigVersion = dpwRes.driverPoolResult.backendConfigVersion,
                backendAppVersion = Just deploymentVersion.getDeploymentVersion,
                isForwardRequest = dpwRes.isForwardRequest,
                previousDropGeoHash = dpwRes.previousDropGeoHash,
                driverTags = Just dpRes.driverTags,
                customerTags = dpRes.customerTags,
                poolingLogicVersion = searchReq.poolingLogicVersion,
                poolingConfigVersion = searchReq.poolingConfigVersion,
                notificationSource = Nothing,
                totalRides = fromMaybe (-1) (driverStats <&> (.totalRides)),
                renderedAt = Nothing,
                respondedAt = Nothing,
                middleStopCount = Just $ length searchReq.stops,
                upgradeCabRequest = Just tripQuoteDetail.eligibleForUpgrade,
                isFavourite = isFavourite,
                parcelType = searchReq.parcelType,
                parcelQuantity = searchReq.parcelQuantity,
                driverTagScore = dpwRes.score,
                ..
              }
      pure searchRequestForDriver

buildTranslatedSearchReqLocation :: (TranslateFlow m r, EsqDBFlow m r, CacheFlow m r) => DLoc.Location -> Maybe Maps.Language -> m DLoc.Location
buildTranslatedSearchReqLocation DLoc.Location {..} mbLanguage = do
  areaRegional <- case mbLanguage of
    Nothing -> return address.area
    Just lang -> do
      mAreaObj <- translate ENGLISH lang `mapM` address.area
      let translation = (\areaObj -> listToMaybe areaObj._data.translations) =<< mAreaObj
      return $ (.translatedText) <$> translation
  pure
    DLoc.Location
      { address =
          DLoc.LocationAddress
            { area = areaRegional,
              street = address.street,
              door = address.door,
              city = address.city,
              state = address.state,
              country = address.country,
              building = address.building,
              areaCode = address.areaCode,
              fullAddress = address.fullAddress,
              instructions = Nothing,
              extras = Nothing
            },
        ..
      }

translateSearchReq ::
  ( TranslateFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DSR.SearchRequest ->
  Maps.Language ->
  m DSR.SearchRequest
translateSearchReq DSR.SearchRequest {..} language = do
  from <- buildTranslatedSearchReqLocation fromLocation (Just language)
  to <- (\loc -> buildTranslatedSearchReqLocation loc (Just language)) `mapM` toLocation
  pure
    DSR.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

addLanguageToDictionary ::
  ( TranslateFlow m r,
    CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r
  ) =>
  DSR.SearchRequest ->
  LanguageDictionary ->
  SDP.DriverPoolWithActualDistResult ->
  m LanguageDictionary
addLanguageToDictionary searchReq dict dPoolRes = do
  let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
  transporterConfig <- SCTC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId))) >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
  if language `elem` transporterConfig.languagesToBeTranslated
    then
      if isJust $ M.lookup language dict
        then return dict
        else do
          translatedSearchReq <- translateSearchReq searchReq language
          pure $ M.insert language translatedSearchReq dict
    else return dict
