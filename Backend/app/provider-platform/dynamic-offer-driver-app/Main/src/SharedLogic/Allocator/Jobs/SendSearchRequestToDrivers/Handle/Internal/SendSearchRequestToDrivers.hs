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

import qualified BecknV2.OnDemand.Utils.Common as BecknUtils
import Control.Applicative ((<|>))
import Control.Monad.Extra (anyM)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.ConditionalCharges as DCC
import Domain.Types.DriverPoolConfig
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person (Driver)
import qualified Domain.Types.Plan as DPlan
import Domain.Types.RiderDetails
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.TransporterConfig as DTR
import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Functions
import qualified Kernel.External.Maps as EMaps
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion (..))
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.DriverCoins.Types as DCT
import Lib.Scheduler.Environment
import Lib.Yudhishthira.Types
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.DriverPool as SDP
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate
import qualified SharedLogic.Type as SLT
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.DomainDiscountConfig as CQDDC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Coins.CoinsConfig as CoinsConfig (fetchCoinConfigByFunctionAndMerchant)
import qualified Storage.Queries.DriverPlan as QDP
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
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    LT.HasLocationService m r,
    JobCreator r m,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
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
  -- DS.driverScoreEventHandler
  --   searchReq.merchantOperatingCityId
  --   DST.OnNewSearchRequestForDrivers
  --     { driverPool = driverPool,
  --       merchantId = searchReq.providerId,
  --       searchReq = searchReq,
  --       searchTry = searchTry,
  --       validTill = validTill,
  --       batchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
  --     }

  -- This is a cache for coin configurations by vehicle category
  coinConfigCache <-
    if isContainsGoldTierTag searchReq.customerNammaTags && fromMaybe 0 searchReq.estimatedDistance > 1000
      then do
        -- This is a map of vehicle categories to coin configurations
        let vehicleCategories = List.nub $ map (\tqd -> BecknUtils.castVehicleCategoryToDomain $ BecknUtils.mapServiceTierToCategory tqd.vehicleServiceTier) tripQuoteDetails
        coinConfigs <- forM vehicleCategories $ \vehicleCategory -> do
          maybeCoinsConfig <-
            CoinsConfig.fetchCoinConfigByFunctionAndMerchant
              DCT.GoldTierRideCompleted
              searchReq.providerId
              searchReq.merchantOperatingCityId
              (Just vehicleCategory)
          return (vehicleCategory, maybeCoinsConfig >>= (\config -> Just config.coins))
        return $ M.fromList coinConfigs
      else return M.empty

  transporterConfig <- SCTC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId))) >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq tripQuoteDetailsHashMap batchNumber validTill transporterConfig searchReq.riderId coinConfigCache) driverPool
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
    let safetyCharges = maybe 0 DCC.charge $ find (\ac -> DCC.SAFETY_PLUS_CHARGES == ac.chargeCategory) tripQuoteDetail.conditionalCharges
    let entityData = USRD.makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq searchTry bapMetadata dPoolRes.intelligentScores.rideRequestPopupDelayDuration dPoolRes.specialZoneExtraTip dPoolRes.keepHiddenForSeconds tripQuoteDetail.vehicleServiceTier needTranslation isValueAddNP useSilentFCMForForwardBatch tripQuoteDetail.driverPickUpCharge tripQuoteDetail.driverParkingCharge safetyCharges tripQuoteDetail.congestionCharges tripQuoteDetail.petCharges tripQuoteDetail.priorityCharges tripQuoteDetail.tollCharges
    -- Notify.notifyOnNewSearchRequestAvailable searchReq.merchantOperatingCityId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
    notificationData <- Notify.buildSendSearchRequestNotificationData searchTry.merchantOperatingCityId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData EmptyDynamicParam (Just searchTry.tripCategory)
    let otherMerchantIds = [Just (Id "840327a8-f17c-4d7c-8199-a583cfaadc5f"), Just (Id "7e6a2982-f8b5-4c67-b8af-bf41f1b4a2c9"), Just (Id "8c91f173-a0e3-4c5b-b3a1-2a58d00f29b2")] :: [Maybe (Id DM.Merchant)] -- Array Contents are : [Dev/Master , UAT , Prod]
    let fallBackCity =
          bool
            (Notify.getNewMerchantOpCityId sReqFD.clientSdkVersion sReqFD.merchantOperatingCityId)
            (Notify.cityFallback sReqFD.clientSdkVersion sReqFD.merchantOperatingCityId)
            (searchTry.merchantId `elem` otherMerchantIds) -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
    Notify.sendSearchRequestToDriverNotification searchReq.providerId fallBackCity sReqFD.driverId notificationData

  -- Update operator/fleet analytics: batch increment total request count for all drivers at once
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    let allDriverIds = map (.driverId) searchRequestsForDrivers
    Analytics.updateOperatorAnalyticsTotalRequestCountBatch allDriverIds transporterConfig
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
      mbDomainDiscountPct <- CQDDC.resolveDomainDiscountPercentage searchReq.merchantOperatingCityId searchTry.emailDomain searchTry.billingCategory farePolicy.vehicleServiceTier
      let farePolicy' =
            farePolicy
              { DFP.businessDiscountPercentage = mbDomainDiscountPct <|> farePolicy.businessDiscountPercentage,
                DFP.personalDiscountPercentage = mbDomainDiscountPct <|> farePolicy.personalDiscountPercentage
              } ::
              DFP.FullFarePolicy
      fareParams <-
        Fare.calculateFareParameters
          Fare.CalculateFareParametersParams
            { farePolicy = farePolicy',
              actualDistance = searchReq.estimatedDistance,
              estimatedDistance = searchReq.estimatedDistance,
              rideTime = searchReq.startTime,
              returnTime = searchReq.returnTime,
              roundTrip = fromMaybe False searchReq.roundTrip,
              waitingTime = Nothing,
              stopWaitingTimes = [],
              actualRideDuration = Nothing,
              petCharges = tripQuoteDetail.petCharges,
              shouldApplyBusinessDiscount = searchTry.billingCategory == SLT.BUSINESS,
              shouldApplyPersonalDiscount = searchTry.billingCategory == SLT.PERSONAL,
              noOfStops = length searchReq.stops,
              estimatedRideDuration = searchReq.estimatedDuration,
              estimatedCongestionCharge = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = Nothing,
              nightShiftCharge = Nothing,
              customerCancellationDues = searchReq.customerCancellationDues,
              nightShiftOverlapChecking = DTC.isFixedNightCharge tripQuoteDetail.tripCategory,
              timeDiffFromUtc = Just transporterConfig.timeDiffFromUtc,
              tollCharges = Nothing,
              vehicleAge = vehicleAge,
              currency = searchReq.currency,
              distanceUnit = searchReq.distanceUnit,
              merchantOperatingCityId = Just searchReq.merchantOperatingCityId,
              mbAdditonalChargeCategories = Nothing,
              numberOfLuggages = searchReq.numberOfLuggages
            }
      pure $ Fare.fareSum fareParams $ Just []

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
        CacheFlow m r,
        HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
        HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
        HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
        HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
      ) =>
      DSR.SearchRequest ->
      HashMap.HashMap DVST.ServiceTierType SDP.TripQuoteDetail ->
      Int ->
      UTCTime ->
      DTR.TransporterConfig ->
      Maybe (Id RiderDetails) ->
      M.Map DTV.VehicleCategory (Maybe Int) ->
      SDP.DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver searchReq tripQuoteDetailsHashMap batchNumber defaultValidTill transporterConfig riderId coinConfigCache dpwRes = do
      let currency = searchTry.currency
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      driverStats <- runInReplica $ QDriverStats.findById dpRes.driverId
      driverPlanSafetyPlus <- QDP.findByDriverIdWithServiceName dpwRes.driverPoolResult.driverId (DPlan.DASHCAM_RENTAL DPlan.CAUTIO)
      tripQuoteDetail <- HashMap.lookup dpRes.serviceTier tripQuoteDetailsHashMap & fromMaybeM (VehicleServiceTierNotFound $ show dpRes.serviceTier)
      let isEligibleForSafetyPlusCharge = maybe False (.enableServiceUsageCharge) driverPlanSafetyPlus && searchReq.preferSafetyPlus
          additionalChargesEligiblFor = additionalChargeConditional isEligibleForSafetyPlusCharge tripQuoteDetail.conditionalCharges
          additionalCharges = sum $ map (\ac -> if ac.chargeCategory `elem` additionalChargesEligiblFor then ac.charge else 0.0) tripQuoteDetail.conditionalCharges
      parallelSearchRequestCount <- Just <$> SDP.getValidSearchRequestCount searchReq.providerId dpRes.driverId now

      let vehicleCategory = BecknUtils.castVehicleCategoryToDomain $ BecknUtils.mapVariantToVehicle dpRes.variant
      let driverCoinsRewardedOnGoldTierRideRequest = join $ M.lookup vehicleCategory coinConfigCache

      logInfo $ "Coins rewarded on gold tier ride request: " <> show driverCoinsRewardedOnGoldTierRideRequest

      baseFare <- case tripQuoteDetail.tripCategory of
        DTC.Ambulance _ -> do
          farePolicy <- getFarePolicyByEstOrQuoteId (Just $ EMaps.getCoordinates searchReq.fromLocation) (Just . EMaps.getCoordinates =<< searchReq.toLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration searchReq.merchantOperatingCityId tripQuoteDetail.tripCategory dpRes.serviceTier searchReq.area searchTry.estimateId Nothing Nothing searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId))) searchReq.configInExperimentVersions searchReq.specialLocationName
          getBaseFare searchReq farePolicy dpRes.vehicleAge tripQuoteDetail transporterConfig
        _ -> pure $ tripQuoteDetail.baseFare + additionalCharges
      deploymentVersion <- asks (.version)
      isFavourite <- maybe (pure Nothing) (\riderid -> findByRiderIdAndDriverId riderid (cast dpRes.driverId) <&> fmap (.favourite)) riderId
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                requestId = searchReq.id,
                searchTryId = searchTry.id,
                vehicleCategory = searchTry.vehicleCategory,
                estimateId = Just tripQuoteDetail.estimateOrQuoteId,
                startTime = searchTry.startTime,
                merchantId = Just searchReq.providerId,
                fromLocGeohash = searchReq.fromLocGeohash,
                tripEstimatedDistance = searchReq.estimatedDistance,
                tripEstimatedDuration = searchReq.estimatedDuration,
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
                reactBundleVersion = dpwRes.driverPoolResult.reactBundleVersion,
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
                conditionalCharges = additionalChargesEligiblFor,
                isSafetyPlus = Just isEligibleForSafetyPlusCharge,
                coinsRewardedOnGoldTierRide = driverCoinsRewardedOnGoldTierRideRequest,
                commissionCharges = tripQuoteDetail.commissionCharges,
                ..
              }
      pure searchRequestForDriver
      where
        additionalChargeConditional isEligibleForSafetyPlusCharge conditionalCharges = do
          let safetyCharges = if isEligibleForSafetyPlusCharge then find (\ac -> ac == DAC.SAFETY_PLUS_CHARGES) $ map (.chargeCategory) conditionalCharges else Nothing
          catMaybes $ [safetyCharges]

    isContainsGoldTierTag :: Maybe [Lib.Yudhishthira.Types.TagNameValue] -> Bool
    isContainsGoldTierTag customerNammaTags =
      case customerNammaTags of
        Just tags -> any (\tag -> tag == TagNameValue "CustomerTier#Gold") tags
        Nothing -> False

buildTranslatedSearchReqLocation :: (TranslateFlow m r, EsqDBFlow m r, CacheFlow m r) => DLoc.Location -> Maybe Maps.Language -> m DLoc.Location
buildTranslatedSearchReqLocation DLoc.Location {..} mbLanguage = do
  areaRegional <- case mbLanguage of
    Nothing -> return address.area
    Just lang -> do
      mAreaObj <- translate ENGLISH lang `mapM` address.area
      let translation = mAreaObj >>= \areaObj -> listToMaybe areaObj._data.translations
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
