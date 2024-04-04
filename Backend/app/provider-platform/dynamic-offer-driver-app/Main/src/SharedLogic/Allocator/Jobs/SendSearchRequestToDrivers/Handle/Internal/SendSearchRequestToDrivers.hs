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
import Domain.Types.DriverPoolConfig
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Location as DLoc
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion (..))
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import qualified SharedLogic.DriverPool as SDP
import SharedLogic.GoogleTranslate
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

type LanguageDictionary = M.Map Maps.Language DSR.SearchRequest

sendSearchRequestToDrivers ::
  ( Log m,
    KvDbFlow m r,
    Esq.EsqDBReplicaFlow m r,
    TranslateFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int, "version" ::: DeploymentVersion]
  ) =>
  [SDP.TripQuoteDetail] ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  DriverPoolConfig ->
  [SDP.DriverPoolWithActualDistResult] ->
  [Id Driver] ->
  GoHomeConfig ->
  m ()
sendSearchRequestToDrivers tripQuoteDetails searchReq searchTry driverPoolConfig driverPool prevBatchDrivers goHomeConfig = do
  logInfo $ "Send search requests to driver pool batch-" <> show driverPool
  bapMetadata <- CQSM.findById (Id searchReq.bapId)
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

  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver tripQuoteDetailsHashMap batchNumber validTill) driverPool
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  whenM (anyM (\driverId -> CQDGR.getDriverGoHomeRequestInfo driverId searchReq.merchantOperatingCityId (Just goHomeConfig) <&> isNothing . (.status)) prevBatchDrivers) $ QSRD.setInactiveBySTId searchTry.id -- inactive previous request by drivers so that they can make new offers.
  _ <- QSRD.createMany searchRequestsForDrivers

  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    transporterConfig <- SCTC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId))) >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
    let needTranslation = language `elem` transporterConfig.languagesToBeTranslated
    let translatedSearchReq =
          if needTranslation
            then fromMaybe searchReq $ M.lookup language languageDictionary
            else searchReq
    isValueAddNP <- CQVAN.isValueAddNP searchReq.bapId
    tripQuoteDetail <- HashMap.lookup dPoolRes.driverPoolResult.serviceTier tripQuoteDetailsHashMap & fromMaybeM (VehicleServiceTierNotFound $ show dPoolRes.driverPoolResult.serviceTier)
    let entityData = USRD.makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq searchTry bapMetadata dPoolRes.intelligentScores.rideRequestPopupDelayDuration dPoolRes.specialZoneExtraTip dPoolRes.keepHiddenForSeconds tripQuoteDetail.vehicleServiceTier needTranslation isValueAddNP tripQuoteDetail.driverPickUpCharge
    -- Notify.notifyOnNewSearchRequestAvailable searchReq.merchantOperatingCityId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
    notificationData <- Notify.buildSendSearchRequestNotificationData sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData Notify.EmptyDynamicParam
    let fallBackCity = Notify.getNewMerchantOpCityId sReqFD.clientSdkVersion sReqFD.merchantOperatingCityId
    Notify.sendSearchRequestToDriverNotification searchReq.providerId fallBackCity notificationData
  where
    getSearchRequestValidTill = do
      now <- getCurrentTime
      let singleBatchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      return $ singleBatchProcessTime `addUTCTime` now
    buildSearchRequestForDriver ::
      ( MonadFlow m,
        Redis.HedisFlow m r,
        HasFlowEnv m r '["version" ::: DeploymentVersion],
        KvDbFlow m r,
        CacheFlow m r
      ) =>
      HashMap.HashMap DVST.ServiceTierType SDP.TripQuoteDetail ->
      Int ->
      UTCTime ->
      SDP.DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver tripQuoteDetailsHashMap batchNumber defaultValidTill dpwRes = do
      let currency = searchTry.currency
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      tripQuoteDetail <- HashMap.lookup dpRes.serviceTier tripQuoteDetailsHashMap & fromMaybeM (VehicleServiceTierNotFound $ show dpRes.serviceTier)
      parallelSearchRequestCount <- Just <$> SDP.getValidSearchRequestCount searchReq.providerId dpRes.driverId now
      deploymentVersion <- asks (.version)
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                requestId = searchReq.id,
                searchTryId = searchTry.id,
                estimateId = Just tripQuoteDetail.estimateOrQuoteId,
                startTime = searchTry.startTime,
                merchantId = Just searchReq.providerId,
                merchantOperatingCityId = searchReq.merchantOperatingCityId,
                searchRequestValidTill = if dpwRes.pickupZone then addUTCTime (fromIntegral dpwRes.keepHiddenForSeconds) defaultValidTill else defaultValidTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                vehicleServiceTier = tripQuoteDetail.vehicleServiceTier,
                vehicleServiceTierName = Just tripQuoteDetail.vehicleServiceTierName,
                airConditioned = (> 0.0) <$> dpRes.airConditioned,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                createdAt = now,
                response = Nothing,
                driverMinExtraFee = tripQuoteDetail.driverMinFee,
                driverMaxExtraFee = tripQuoteDetail.driverMaxFee,
                driverStepFee = tripQuoteDetail.driverStepFee,
                driverDefaultStepFee = tripQuoteDetail.driverDefaultStepFee,
                rideRequestPopupDelayDuration = dpwRes.intelligentScores.rideRequestPopupDelayDuration,
                baseFare = Just tripQuoteDetail.baseFare,
                currency,
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
                ..
              }
      pure searchRequestForDriver

buildTranslatedSearchReqLocation :: (TranslateFlow m r, KvDbFlow m r) => DLoc.Location -> Maybe Maps.Language -> m DLoc.Location
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
              fullAddress = address.fullAddress
            },
        ..
      }

translateSearchReq ::
  ( TranslateFlow m r,
    KvDbFlow m r
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
    KvDbFlow m r,
    EncFlow m r
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
