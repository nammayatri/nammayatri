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
    buildSearchRequestForDriver,
    attemptPriorityDirectAssign,
  )
where

import qualified BecknV2.OnDemand.Utils.Common as BecknUtils
import Control.Applicative ((<|>))
import qualified Control.Monad.Catch as C
import Control.Monad.Extra (anyM)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Map as Map
import Domain.Action.UI.Driver (AcceptDynamicOfferFlow, acceptDynamicOfferDriverRequest)
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.Booking as DRB
import Domain.Types.Common
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
import qualified Domain.Types.VehicleServiceTier as VST
-- import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Functions
import qualified Kernel.External.Maps as EMaps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics, DeploymentVersion (..))
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as LDS
import qualified Lib.DriverScore.Types as LDST
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler.Environment
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified Lib.Types.SpecialLocation as SL
import Lib.Yudhishthira.Types
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPoolUnified as UI
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.DriverIdleTime as DriverIdleTime
import qualified SharedLogic.DriverPool as SDP
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as Fare
import SharedLogic.FarePolicy
import SharedLogic.Finance.Wallet (addOfferHoldsForSearchTry)
import SharedLogic.GoogleTranslate
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.Ride (offerQuoteLockKeyWithCoolDown)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified SharedLogic.Type as SLT
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.DomainDiscountConfig as CQDDC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.CoinsConfig (CoinsConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.BookingExtra as QBE
import qualified Storage.Queries.Coins.CoinsConfig as SQCC
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import Storage.Queries.Person.GetNearestDrivers (isDriverModeEligibleHelper)
import Storage.Queries.RiderDriverCorrelation
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Metrics as TM
import qualified Tools.Notifications as Notify
import TransactionLogs.Types (KeyConfig, TokenConfig)
import Utils.Common.Cac.KeyNameConstants

type LanguageDictionary = M.Map Maps.Language DSR.SearchRequest

sendSearchRequestToDrivers ::
  ( Log m,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    TranslateFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int, "version" ::: DeploymentVersion, "bppMetrics" ::: TM.BPPMetricsContainer],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds,
    TM.HasDriverSearchRequestResponseMetrics m r,
    EncFlow m r,
    LT.HasLocationService m r,
    JobCreator r m,
    HasShortDurationRetryCfg r c,
    HasHttpClientOptions r c,
    HasKafkaProducer r,
    ClickhouseFlow m r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    BeamFlow m r,
    CoreMetrics m,
    EventStreamFlow m r,
    HasPrettyLogger m r,
    ServiceFlow m r,
    C.MonadCatch m
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

  -- This is a cache for coin configurations by service tier type
  coinConfigCache <-
    if isContainsGoldTierTag searchReq.customerNammaTags && fromMaybe 0 searchReq.estimatedDistance > 1000
      then do
        let serviceTiers = List.nub $ map (.vehicleServiceTier) tripQuoteDetails
        coinConfigs <- forM serviceTiers $ \stt -> do
          let vehicleCategory = BecknUtils.castVehicleCategoryToDomain $ BecknUtils.mapServiceTierToCategory stt
          maybeCoinsConfig <-
            getOneConfig (CoinsConfigDimensions {merchantOptCityId = searchReq.merchantOperatingCityId.getId, eventFunction = Just DCT.GoldTierRideCompleted, merchantId = Just searchReq.providerId.getId, active = Just True, vehicleCategory = Just vehicleCategory, serviceTierType = Just stt, eventName = Nothing, tripCategoryType = Nothing, configId = Nothing}) (Just (maybeToList <$> SQCC.fetchCoinConfigByFunctionAndMerchant DCT.GoldTierRideCompleted searchReq.providerId searchReq.merchantOperatingCityId (Just vehicleCategory) (Just stt)))
          return (stt, maybeCoinsConfig >>= (\config -> Just config.coins))
        return $ M.fromList coinConfigs
      else return M.empty
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = searchReq.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
  cityServiceTiers <- CQVST.findAllByMerchantOpCityIdInRideFlow searchReq.merchantOperatingCityId (searchReq.area >>= SL.pickupSpecialZoneIdFromArea)
  dispatchPool <- attemptPriorityDirectAssign merchant searchReq searchTry tripQuoteDetails cityServiceTiers driverPoolConfig batchNumber transporterConfig coinConfigCache driverPool
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty dispatchPool
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchTry searchReq tripQuoteDetailsHashMap batchNumber validTill transporterConfig searchReq.riderId coinConfigCache False) dispatchPool
  let driverPoolZipSearchRequests = zip dispatchPool searchRequestsForDrivers
  (merchantLabel, cityLabel) <- SML.getMetricsLabels searchReq.providerId searchReq.merchantOperatingCityId
  let metricsDistanceBucketEdges = SML.distanceBucketEdges transporterConfig
  -- Previous batch's still-active unresponded requests, fetched once: shared by the
  -- special-zone queue-skip fork and the expired-request accounting below. Fetched
  -- before createMany so the new batch's rows can't leak in.
  unrespondedSRFDs <-
    if isJust searchReq.pickupZoneGateId || not (null prevBatchDrivers)
      then filter (isNothing . (.response)) <$> QSRD.findAllActiveBySTId searchTry.id Active
      else pure []
  -- Handle queue skip for timed-out special zone drivers before marking them inactive.
  -- Forked because it's independent of the search-batch flow — failures must not
  -- block driver dispatch or add latency to the allocator.
  when (isJust searchReq.pickupZoneGateId) $
    fork "specialZoneQueueSkipForTimedOutDrivers" $ do
      let timedOutQueueDrivers = filter (.pickupZone) unrespondedSRFDs
      forM_ timedOutQueueDrivers $ \srfd ->
        SpecialZoneDriverDemand.handleQueueSkipIfApplicable searchReq.pickupZoneGateId (show searchTry.vehicleServiceTier) srfd.driverId searchReq.providerId (searchTry.id.getId <> ":" <> srfd.driverId.getId)
  let dispatchDriverIds = map (\dp -> dp.driverPoolResult.driverId.getId) dispatchPool
      reOfferedSRFDs = filter (\srfd -> srfd.driverId.getId `elem` dispatchDriverIds) unrespondedSRFDs
  unless (null reOfferedSRFDs) $
    whenM (anyM (\driverId -> CQDGR.getDriverGoHomeRequestInfo driverId searchReq.merchantOperatingCityId (Just goHomeConfig) <&> isNothing . (.status)) prevBatchDrivers) $ do
      -- these unresponded requests are being retracted here: count them as expired
      forM_ (M.toList $ M.fromListWith (+) $ map (\srfd -> (srfd.vehicleServiceTier, 1 :: Int)) reOfferedSRFDs) $ \(serviceTier, expiredCount) ->
        TM.addSearchRequestExpiredCount merchantLabel cityLabel (show serviceTier) (SML.searchReqFunnelLabels metricsDistanceBucketEdges searchReq) expiredCount
      QSRD.setInactiveAndPulledByIds reOfferedSRFDs
  _ <- QSRD.createMany searchRequestsForDrivers
  -- Batch size on record, so the respond API can recognise a *fully* rejected batch
  -- (rejects == sent) and advance the batch chain early instead of idling out the timer.
  SDP.setBatchSentCount searchTry.id batchNumber (length searchRequestsForDrivers)
  forM_ (M.toList $ M.fromListWith (+) $ map (\srfd -> (srfd.vehicleServiceTier, 1 :: Int)) searchRequestsForDrivers) $ \(serviceTier, sentCount) ->
    TM.addSearchRequestSentToDriverCount merchantLabel cityLabel (show serviceTier) (SML.searchReqFunnelLabels metricsDistanceBucketEdges searchReq) sentCount

  -- Count one "request sent" per driver in this batch for the SRDStats sliding-window counters
  -- and reset each driver's idle clock, both surfaced in the POOLING dynamic-logic data.
  forM_ dispatchPool $ \dPoolRes -> do
    let personId = cast dPoolRes.driverPoolResult.driverId
    SDP.incrementSrdSentCount personId
    DriverIdleTime.resetIdleOnRequestSent personId

  isValueAddNP <- CQVAN.isValueAddNP searchReq.bapId
  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    let needTranslation = language `elem` transporterConfig.languagesToBeTranslated
    let translatedSearchReq =
          if needTranslation
            then fromMaybe searchReq $ M.lookup language languageDictionary
            else searchReq
    let useSilentFCMForForwardBatch = transporterConfig.useSilentFCMForForwardBatch
    tripQuoteDetail <- HashMap.lookup dPoolRes.driverPoolResult.serviceTier tripQuoteDetailsHashMap & fromMaybeM (VehicleServiceTierNotFound $ show dPoolRes.driverPoolResult.serviceTier)
    let holdOwnerId = fromMaybe dPoolRes.driverPoolResult.driverId.getId dPoolRes.driverPoolResult.fleetOwnerId
    addOfferHoldsForSearchTry transporterConfig isPrepaidEnabled holdOwnerId searchTry.id.getId searchTry.paymentInstrument searchTry.baseFare tripQuoteDetail.govtCharges tripQuoteDetail.tollCharges tripQuoteDetail.driverParkingCharge validTill
    let safetyCharges = maybe 0 DCC.charge $ find (\ac -> DCC.SAFETY_PLUS_CHARGES == ac.chargeCategory) tripQuoteDetail.conditionalCharges
    let entityData = USRD.makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq searchTry bapMetadata dPoolRes.intelligentScores.rideRequestPopupDelayDuration dPoolRes.specialZoneExtraTip dPoolRes.keepHiddenForSeconds tripQuoteDetail.vehicleServiceTier needTranslation isValueAddNP useSilentFCMForForwardBatch tripQuoteDetail.driverPickUpCharge tripQuoteDetail.driverParkingCharge safetyCharges tripQuoteDetail.congestionCharges tripQuoteDetail.petCharges tripQuoteDetail.priorityCharges tripQuoteDetail.tollCharges (Just transporterConfig.driverWalletConfig)
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
    getSearchRequestValidTill = do
      now <- getCurrentTime
      let singleBatchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      return $ singleBatchProcessTime `addUTCTime` now
    isContainsGoldTierTag :: Maybe [Lib.Yudhishthira.Types.TagNameValue] -> Bool
    isContainsGoldTierTag customerNammaTags =
      case customerNammaTags of
        Just tags -> any (\tag -> tag == TagNameValue "CustomerTier#Gold") tags
        Nothing -> False

getBaseFare ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CacheFlow m r
  ) =>
  DST.SearchTry ->
  DSR.SearchRequest ->
  DFP.FullFarePolicy ->
  Maybe Months ->
  SDP.TripQuoteDetail ->
  DTR.TransporterConfig ->
  m HighPrecMoney
getBaseFare searchTry searchReq farePolicy vehicleAge tripQuoteDetail transporterConfig = do
  mbDomainDiscountPct <- CQDDC.resolveDomainDiscountPercentage searchReq.merchantOperatingCityId searchTry.emailDomain searchTry.businessEmailDomain searchTry.billingCategory farePolicy.vehicleServiceTier
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
          estimatedRideStaticDuration = searchReq.estimatedStaticDuration,
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
          numberOfLuggages = searchReq.numberOfLuggages,
          govtChargesRate = Just transporterConfig.taxConfig.rideGst,
          pickupGateId = searchReq.pickupGateId,
          fareSettlementType = farePolicy'.fareSettlementType
        }
  pure $ Fare.fareSum fareParams $ Just []

-- | Extracted from sendSearchRequestToDrivers' where-clause so DriverPoolUnified's priority-assign path can reuse it too.
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
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    ClickhouseFlow m r
  ) =>
  DST.SearchTry ->
  DSR.SearchRequest ->
  HashMap.HashMap DVST.ServiceTierType SDP.TripQuoteDetail ->
  Int ->
  UTCTime ->
  DTR.TransporterConfig ->
  Maybe (Id RiderDetails) ->
  M.Map DVST.ServiceTierType (Maybe Int) ->
  Bool ->
  SDP.DriverPoolWithActualDistResult ->
  m SearchRequestForDriver
buildSearchRequestForDriver searchTry searchReq tripQuoteDetailsHashMap batchNumber defaultValidTill transporterConfig riderId coinConfigCache isAutoAccepted dpwRes = do
  let currency = searchTry.currency
  guid <- generateGUID
  now <- getCurrentTime
  let dpRes = dpwRes.driverPoolResult
  driverStats <- runInReplica $ QDriverStats.findById dpRes.driverId
  driverPlanSafetyPlus <- QDP.findByDriverIdWithServiceName dpRes.driverId (DPlan.DASHCAM_RENTAL DPlan.CAUTIO)
  tripQuoteDetail <- HashMap.lookup dpRes.serviceTier tripQuoteDetailsHashMap & fromMaybeM (VehicleServiceTierNotFound $ show dpRes.serviceTier)
  let isEligibleForSafetyPlusCharge = maybe False (.enableServiceUsageCharge) driverPlanSafetyPlus && searchReq.preferSafetyPlus
      additionalChargesEligiblFor = additionalChargeConditional isEligibleForSafetyPlusCharge tripQuoteDetail.conditionalCharges
      additionalCharges = sum $ map (\ac -> if ac.chargeCategory `elem` additionalChargesEligiblFor then ac.charge else 0.0) tripQuoteDetail.conditionalCharges
  parallelSearchRequestCount <- Just <$> SDP.getValidSearchRequestCount searchReq.providerId dpRes.driverId now

  let driverCoinsRewardedOnGoldTierRideRequest = join $ M.lookup dpRes.serviceTier coinConfigCache

  logInfo $ "Coins rewarded on gold tier ride request: " <> show driverCoinsRewardedOnGoldTierRideRequest

  baseFare <- case tripQuoteDetail.tripCategory of
    DTC.Ambulance _ -> do
      farePolicy <- getFarePolicyByEstOrQuoteId (Just $ EMaps.getCoordinates searchReq.fromLocation) (Just . EMaps.getCoordinates =<< searchReq.toLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration searchReq.merchantOperatingCityId tripQuoteDetail.tripCategory dpRes.serviceTier searchReq.area searchTry.estimateId Nothing Nothing searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId))) searchReq.configInExperimentVersions searchReq.specialLocationName
      getBaseFare searchTry searchReq farePolicy dpRes.vehicleAge tripQuoteDetail transporterConfig
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
            fleetOwnerId = Id <$> dpRes.fleetOwnerId,
            vehicleNumber = dpRes.vehicleNumber,
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
            clientSdkVersion = dpRes.clientSdkVersion,
            reactBundleVersion = dpRes.reactBundleVersion,
            clientBundleVersion = dpRes.clientBundleVersion,
            clientConfigVersion = dpRes.clientConfigVersion,
            clientDevice = dpRes.clientDevice,
            backendConfigVersion = dpRes.backendConfigVersion,
            backendAppVersion = Just deploymentVersion.getDeploymentVersion,
            isForwardRequest = dpwRes.isForwardRequest,
            previousDropGeoHash = dpwRes.previousDropGeoHash,
            driverTags = Just $ addSpecialLocWarriorPreferredSpecialLocId dpwRes.specialLocWarriorPreferredSpecialLocId dpRes.driverTags,
            customerTags = dpRes.customerTags,
            poolingLogicVersion = dpwRes.poolingLogicVersion <|> searchReq.poolingLogicVersion,
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
            preferenceMatchScore = Just dpwRes.preferenceMatchScore,
            conditionalCharges = additionalChargesEligiblFor,
            isSafetyPlus = Just isEligibleForSafetyPlusCharge,
            coinsRewardedOnGoldTierRide = driverCoinsRewardedOnGoldTierRideRequest,
            commissionCharges = tripQuoteDetail.commissionCharges,
            driverCancellationNotAllowed = tripQuoteDetail.driverCancellationNotAllowed,
            isAutoAccepted = Just isAutoAccepted,
            ..
          }
  pure searchRequestForDriver
  where
    addSpecialLocWarriorPreferredSpecialLocId mbSpecialLocId driverTags =
      case mbSpecialLocId of
        Nothing -> driverTags
        Just specialLocId ->
          let tagKey = AK.fromString "SpecialLocWarriorPreferredSpecialLoc"
              tagValue = String specialLocId.getId
           in case driverTags of
                Object keymap -> Object $ AKM.insert tagKey tagValue keymap
                _ -> Object $ AKM.singleton tagKey tagValue

    additionalChargeConditional isEligibleForSafetyPlusCharge conditionalCharges = do
      let safetyCharges = if isEligibleForSafetyPlusCharge then find (\ac -> ac == DAC.SAFETY_PLUS_CHARGES) $ map (.chargeCategory) conditionalCharges else Nothing
      catMaybes $ [safetyCharges]

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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = searchReq.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
  if language `elem` transporterConfig.languagesToBeTranslated
    then
      if isJust $ M.lookup language dict
        then return dict
        else do
          translatedSearchReq <- translateSearchReq searchReq language
          pure $ M.insert language translatedSearchReq dict
    else return dict

-- | Before broadcast, try to silently direct-assign one AutoAssign#<tier>-tagged driver nearest-first; on success returns [], else the batch unchanged.
attemptPriorityDirectAssign ::
  forall m r c.
  ( AcceptDynamicOfferFlow m r c,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds,
    TM.HasDriverSearchRequestResponseMetrics m r,
    EncFlow m r,
    JobCreator r m,
    LT.HasLocationService m r,
    C.MonadCatch m
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  [SDP.TripQuoteDetail] ->
  [VST.VehicleServiceTier] ->
  DriverPoolConfig ->
  SDP.PoolBatchNum ->
  DTR.TransporterConfig ->
  Map.Map DVST.ServiceTierType (Maybe Int) ->
  [SDP.DriverPoolWithActualDistResult] ->
  m [SDP.DriverPoolWithActualDistResult]
attemptPriorityDirectAssign merchant searchReq searchTry tripQuoteDetails cityServiceTiers driverPoolCfg batchNum transporterConfig coinConfigCache batch = do
  if null sortedPriority
    then pure batch
    else do
      now <- getCurrentTime
      let validTill = fromIntegral driverPoolCfg.singleBatchProcessTime `addUTCTime` now
      quoteRespondCoolDown <- asks (.quoteRespondCoolDown)
      assigned <- tryAssign validTill quoteRespondCoolDown sortedPriority
      -- On success return [] -- broadcasting the remainder would still notify for an already-taken ride.
      pure $ if assigned then [] else batch
  where
    tripQuoteDetailsHashMap = HashMap.fromList $ (\tqd -> (tqd.vehicleServiceTier, tqd)) <$> tripQuoteDetails
    -- Closes over the outer args; only validTill, the cooldown, and the candidate list vary per call.
    tryAssign :: UTCTime -> Int -> [SDP.DriverPoolWithActualDistResult] -> m Bool
    tryAssign _ _ [] = pure False
    tryAssign validTill quoteRespondCoolDown (dp : rest) = do
      let driverId = cast dp.driverPoolResult.driverId
          unlockThisDriver = Redis.unlockRedis (offerQuoteLockKeyWithCoolDown driverId)
      locked <- Redis.tryLockRedis (offerQuoteLockKeyWithCoolDown driverId) quoteRespondCoolDown
      if not locked
        then tryAssign validTill quoteRespondCoolDown rest
        else do
          result :: Either SomeException Bool <- C.try $ do
            mbFreshPoolData <- listToMaybe <$> DPD.getDriverPoolDataBatch [driverId]
            let stillHasTierSelected = maybe False ((dp.driverPoolResult.serviceTier `elem`) . (.selectedServiceTiers)) mbFreshPoolData
                stillHasAutoAcceptTierSelected = maybe False ((dp.driverPoolResult.serviceTier `elem`) . fromMaybe [] . (.selectedAutoAcceptTiers)) mbFreshPoolData
            -- Deliberately NO dispatch-time wallet balance re-check here: the opt-in write gate
            -- (checkMinWalletBalance in postDriverUpdateServiceTiers) admits only sufficient
            -- balances and the debit-driven auto-revoke strips selections synchronously with
            -- every wallet decrease, so selectedAutoAcceptTiers + the cohort tag together are
            -- treated as the standing guarantee of eligibility. A stale selection in the ms
            -- window between debit commit and revoke strip is the accepted trade-off, chosen
            -- over one wallet DB read per candidate in the dispatch hot loop.
            let isStillLive =
                  maybe False (\d -> not d.blocked && d.enabled && not d.isDisabledReasonFlag && d.subscribed && isDriverModeEligibleHelper d.mode d.active) mbFreshPoolData
                    && stillHasTierSelected
                    && stillHasAutoAcceptTierSelected
                -- No LTS entry at all reads as on-ride/unavailable, never as eligible.
                onRide = maybe True (.onRide) mbFreshPoolData
            -- The Redis lock above expires after quoteRespondCoolDown while a quote stays Active for
            -- driverQuoteExpirationSeconds, so the lock alone does NOT rule out a live quote or a
            -- confirmed booking -- re-run the two DB guards respondQuote's Accept branch enforces.
            driverUnlockDelay <- asks (.driverUnlockDelay)
            activeQuotes <- QDrQt.findActiveQuotesByDriverId driverId driverUnlockDelay
            mbActiveBooking <-
              if DTC.isDynamicOfferTrip searchTry.tripCategory
                then runInMasterRedis $ QBE.findByTransactionIdAndStatuses searchReq.transactionId [DRB.NEW, DRB.TRIP_ASSIGNED]
                else pure Nothing
            if onRide || not isStillLive || not (null activeQuotes) || isJust mbActiveBooking
              then pure False
              else do
                sReqFD <- buildSearchRequestForDriver searchTry searchReq tripQuoteDetailsHashMap batchNum validTill transporterConfig searchReq.riderId coinConfigCache True dp
                -- Nested try: a failure after the SRFD row exists must retract it, otherwise the
                -- driver can still poll and manually accept an offer this loop already abandoned.
                assignResult :: Either SomeException [SearchRequestForDriver] <- C.try $ do
                  QSRD.createMany [sReqFD]
                  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
                  driverStats <- QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
                  driverFCMPulledList <- acceptDynamicOfferDriverRequest Nothing merchant.id searchReq.merchantOperatingCityId merchant searchTry searchReq driver sReqFD Nothing Nothing Nothing Nothing Nothing Nothing driverStats transporterConfig
                  respondedAt <- getCurrentTime
                  QSRD.updateDriverResponse (Just Accept) Inactive Nothing (Just respondedAt) (Just respondedAt) sReqFD.id
                  -- The same post-accept bundle respondQuote runs, so silent and manual accepts
                  -- stay indistinguishable to analytics, funnel metrics and the score/pool counters.
                  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
                    Analytics.updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount driverId transporterConfig False True False False
                  cityLabel <- SML.getCityLabel searchReq.merchantOperatingCityId
                  TM.incrementDriverResponseCounter merchant.shortId.getShortId cityLabel (show sReqFD.vehicleServiceTier) (show sReqFD.batchNumber) (show Accept) (SML.driverSearchReqFunnelLabels (SML.distanceBucketEdges transporterConfig) sReqFD)
                  SDP.recordQuoteResponseCounters searchReq.merchantOperatingCityId driverId Accept
                  pure driverFCMPulledList
                case assignResult of
                  Left err -> do
                    logError $ "attemptPriorityDirectAssign: silent assign failed for driverId " <> driverId.getId <> ", searchTryId " <> searchTry.id.getId <> ": " <> show err
                    QSRD.updateDriverResponse Nothing Inactive Nothing Nothing Nothing sReqFD.id
                    pure False
                  Right driverFCMPulledList -> do
                    LDS.driverScoreEventHandler searchReq.merchantOperatingCityId $
                      LDST.OnDriverAcceptingSearchRequest
                        { merchantId = merchant.id,
                          driverId,
                          searchTryId = searchTry.id,
                          searchReqId = searchReq.id,
                          restDriverIds = map (.driverId) driverFCMPulledList,
                          response = Accept
                        }
                    pure True
          case result of
            Right True -> pure True -- lock stays held; initializeRide releases it (same as manual accept)
            Left err -> do
              logError $ "attemptPriorityDirectAssign: candidate check failed for driverId " <> driverId.getId <> ", searchTryId " <> searchTry.id.getId <> ": " <> show err
              unlockThisDriver >> tryAssign validTill quoteRespondCoolDown rest
            Right False -> unlockThisDriver >> tryAssign validTill quoteRespondCoolDown rest
    autoAcceptanceConfigForTier tier =
      DL.find (\vst -> vst.serviceTierType == tier) cityServiceTiers >>= (.autoAcceptanceConfig)
    isAutoAssignEnabledForTier tier =
      maybe False (.enabled) (autoAcceptanceConfigForTier tier)
    -- Checked against DriverPoolResult's typed field, not a driverTags marker -- avoids encode/decode mismatch.
    isPriorityCandidate dp =
      isAutoAssignEnabledForTier dp.driverPoolResult.serviceTier
        && UI.hasPriorityTag (show dp.driverPoolResult.serviceTier) dp
        && dp.driverPoolResult.serviceTier `elem` dp.driverPoolResult.selectedAutoAcceptTiers
    priorityCandidates = DL.filter isPriorityCandidate batch
    sortedPriority = DL.sortOn (.actualDistanceToPickup) priorityCandidates
