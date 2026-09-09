{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.UI.DriverAcceptOffer
  ( AcceptDynamicOfferFlow,
    acceptDynamicOfferDriverRequest,
    buildDriverQuote,
    isAllowedExtraFee,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Types as DTC
import qualified Domain.Types.DriverQuote as DDrQuote
import qualified Domain.Types.DriverStats as DStats
import qualified Domain.Types.FareParameters as Fare
import Domain.Types.FarePolicy (DriverExtraFeeBounds (..))
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Domain.Types.SearchTry as DST
import Domain.Types.TransporterConfig
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (decodeUtf8, id, state)
import GHC.Records.Extra
import Kernel.Beam.Functions
import Kernel.Beam.Types (TxnIdKey (..))
import qualified Kernel.External.Maps as Maps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude (NominalDiffTime)
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.CallBAP (sendDriverOffer)
import SharedLogic.FareCalculator
import qualified SharedLogic.FareCalculator as FC
import SharedLogic.FarePolicy
import SharedLogic.Pricing
import SharedLogic.Ride
import qualified SharedLogic.SearchTryLocker as CS
import qualified SharedLogic.Type as SLT
import qualified Storage.Cac.DriverPoolConfig as SCDPC
import qualified Storage.CachedQueries.DomainDiscountConfig as CQDDC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event
import TransactionLogs.Types (KeyConfig, TokenConfig)
import Utils.Common.Cac.KeyNameConstants

isAllowedExtraFee :: DriverExtraFeeBounds -> HighPrecMoney -> Bool
isAllowedExtraFee extraFee val = extraFee.minFee <= val && val <= extraFee.maxFee

-- | Everything needed to build a driver quote, cache its fare policy, event-stream it, and offer it to the BAP.
-- Shared by acceptDynamicOfferDriverRequest and every path that replays it server-side (silent-assign, reassignment on cancel/pickup-stall/schedule-update).
type AcceptDynamicOfferFlow m r c =
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    ServiceFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    -- HasFlowEnv, not plain HasField: avoids an overlapping-instance error where a caller also needs buildSearchRequestForDriver's own HasFlowEnv "version".
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    HasKafkaProducer r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    EventStreamFlow m r,
    HasPrettyLogger m r
  )

-- | Extracted from respondQuote's Accept branch so DriverPoolUnified can replay it server-side for a silently-assigned driver.
acceptDynamicOfferDriverRequest ::
  AcceptDynamicOfferFlow m r c =>
  Maybe Text ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DM.Merchant ->
  DST.SearchTry ->
  DSR.SearchRequest ->
  SP.Person ->
  SearchRequestForDriver ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  DStats.DriverStats ->
  TransporterConfig ->
  m [SearchRequestForDriver]
acceptDynamicOfferDriverRequest clientId merchantId merchantOpCityId merchant searchTry searchReq driver sReqFD mbBundleVersion' mbClientVersion' mbConfigVersion' mbReactBundleVersion' mbDevice' reqOfferedValue driverStats transporterConfig = do
  let estimateId = fromMaybe searchTry.estimateId sReqFD.estimateId -- backward compatibility
  logDebug $ "offered fare: " <> show reqOfferedValue
  quoteLimit <- getQuoteLimit searchReq.estimatedDistance sReqFD.vehicleServiceTier searchTry.tripCategory searchReq (fromMaybe SL.Default searchReq.area) searchTry.searchRepeatType searchTry.searchRepeatCounter
  quoteCount <- runInReplica $ QDrQt.countAllBySTId searchTry.id
  when (quoteCount >= quoteLimit) (throwError QuoteAlreadyRejected)
  farePolicy <- getFarePolicyByEstOrQuoteId (Just $ Maps.getCoordinates searchReq.fromLocation) (Just . Maps.getCoordinates =<< searchReq.toLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration merchantOpCityId searchTry.tripCategory sReqFD.vehicleServiceTier searchReq.area estimateId Nothing Nothing searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId))) searchReq.configInExperimentVersions searchReq.specialLocationName
  let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
  whenJust reqOfferedValue $ \off ->
    whenJust driverExtraFeeBounds $ \driverExtraFeeBounds' ->
      unless (isAllowedExtraFee driverExtraFeeBounds' off) $
        throwError $ NotAllowedExtraFee $ show off
  unlessM (validateSearchTryActive searchTry.id) $ do
    logError ("RideRequestAlreadyAcceptedOrCancelled " <> "in respond quote for searchTryId:" <> getId searchTry.id <> " estimateId:" <> estimateId <> " driverId:" <> getId driver.id <> " and srfdId:" <> getId sReqFD.id)
    throwError (RideRequestAlreadyAcceptedOrCancelled sReqFD.id.getId)
  mbDomainDiscountPct <- CQDDC.resolveDomainDiscountPercentage merchantOpCityId searchTry.emailDomain searchTry.businessEmailDomain searchTry.billingCategory farePolicy.vehicleServiceTier
  let farePolicy' =
        farePolicy
          { DFarePolicy.businessDiscountPercentage = mbDomainDiscountPct <|> farePolicy.businessDiscountPercentage,
            DFarePolicy.personalDiscountPercentage = mbDomainDiscountPct <|> farePolicy.personalDiscountPercentage
          } ::
          DFarePolicy.FullFarePolicy
  fareParams <- do
    FC.calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = farePolicy',
          actualDistance = searchReq.estimatedDistance,
          rideTime = sReqFD.startTime,
          returnTime = searchReq.returnTime,
          roundTrip = fromMaybe False searchReq.roundTrip,
          vehicleAge = sReqFD.vehicleAge,
          waitingTime = Nothing,
          stopWaitingTimes = [],
          noOfStops = length searchReq.stops,
          actualRideDuration = Nothing,
          driverSelectedFare = reqOfferedValue,
          customerExtraFee = searchTry.customerExtraFee,
          petCharges = if isJust searchTry.petCharges then farePolicy.petCharges else Nothing,
          nightShiftCharge = Nothing,
          customerCancellationDues = searchReq.customerCancellationDues,
          tollCharges = searchReq.tollCharges,
          estimatedRideDuration = searchReq.estimatedDuration,
          estimatedRideStaticDuration = searchReq.estimatedStaticDuration,
          nightShiftOverlapChecking = DTC.isFixedNightCharge searchTry.tripCategory,
          estimatedCongestionCharge = Nothing,
          estimatedDistance = searchReq.estimatedDistance,
          timeDiffFromUtc = Nothing,
          currency = searchReq.currency,
          shouldApplyBusinessDiscount = searchTry.billingCategory == SLT.BUSINESS,
          shouldApplyPersonalDiscount = searchTry.billingCategory == SLT.PERSONAL,
          distanceUnit = searchReq.distanceUnit,
          merchantOperatingCityId = Just merchantOpCityId,
          mbAdditonalChargeCategories = Just sReqFD.conditionalCharges,
          numberOfLuggages = searchReq.numberOfLuggages,
          govtChargesRate = Just transporterConfig.taxConfig.rideGst,
          pickupGateId = searchReq.pickupGateId,
          fareSettlementType = farePolicy'.fareSettlementType,
          isScheduled = searchTry.isScheduled,
          isManualToll = False,
          ..
        }
  driverQuote <- buildDriverQuote clientId driver driverStats searchReq sReqFD estimateId searchTry.tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbReactBundleVersion' mbDevice'
  void $ cacheFarePolicyByQuoteId driverQuote.id.getId farePolicy
  triggerQuoteEvent QuoteEventData {quote = driverQuote}
  void $ QDrQt.create driverQuote
  driverFCMPulledList <-
    if (quoteCount + 1) >= quoteLimit || (searchReq.autoAssignEnabled == Just True)
      then runInMasterRedis $ QSRD.findAllActiveBySTId searchTry.id DSRD.Active
      else pure []
  pullExistingRideRequests merchantOpCityId driverFCMPulledList merchantId driver.id (mkPrice (Just driverQuote.currency) driverQuote.estimatedFare) transporterConfig
  sendDriverOffer merchant searchReq sReqFD searchTry driverQuote
  return driverFCMPulledList
  where
    getQuoteLimit dist vehicleServiceTier tripCategory sr area searchRepeatType searchRepeatCounter = do
      L.setOptionLocal TxnIdKey sr.transactionId
      driverPoolCfg <- SCDPC.getDriverPoolConfig merchantOpCityId vehicleServiceTier tripCategory area dist searchRepeatType searchRepeatCounter (Just (TransactionId (Id sr.transactionId))) sr
      pure driverPoolCfg.driverQuoteLimit
    validateSearchTryActive searchTryId = do
      -- Lock Description: This is a Lock held between Driver Respond and Cancel Search, if UI Cancel Search is OnGoing then the SearchTry will be marked as CANCELLED and Driver Respond will fail with `RideRequestAlreadyAcceptedOrCancelled`.
      -- Lock Release: Held for 5 seconds once acquired, never released.
      isLockAcquired <- CS.lockSearchTry searchTryId
      if isLockAcquired
        then do
          mbUpdatedSearchTry <- runInMasterDbAndRedis $ QST.findById searchTryId
          return $ maybe True (\updatedSearchTry -> updatedSearchTry.status == DST.ACTIVE) mbUpdatedSearchTry
        else do
          return False

buildDriverQuote ::
  (MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, MonadReader r m, HasField "driverQuoteExpirationSeconds" r NominalDiffTime, HasField "version" r DeploymentVersion) =>
  Maybe Text ->
  SP.Person ->
  DStats.DriverStats ->
  DSR.SearchRequest ->
  SearchRequestForDriver ->
  Text ->
  DTC.TripCategory ->
  Fare.FareParameters ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  m DDrQuote.DriverQuote
buildDriverQuote clientId driver driverStats searchReq sd estimateId tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbReactBundleVersion' mbDevice' = do
  guid <- generateGUID
  now <- getCurrentTime
  deploymentVersion <- asks (.version)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = searchReq.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
  if tripCategory == DTC.OneWay DTC.OneWayOnDemandDynamicOffer && transporterConfig.isDynamicPricingQARCalEnabled == Just True
    then
      fork "updateDynamicPricingAcceptanceCounters" $
        geoAddDynamicPricingCounter mkAcceptanceVehicleCategoryWithDistanceBin mkAcceptanceVehicleCategory mkAcceptanceVehicleCategoryCity now sd.vehicleCategory searchReq.fromLocation.lat searchReq.fromLocation.lon sd.searchTryId.getId ((.getMeters) <$> searchReq.estimatedDistance) searchReq.merchantOperatingCityId.getId
    else pure ()
  driverQuoteExpirationSeconds <- asks (.driverQuoteExpirationSeconds)
  let estimatedFare = fareSum fareParams $ Just sd.conditionalCharges
  pure
    DDrQuote.DriverQuote
      { id = guid,
        requestId = searchReq.id,
        searchTryId = sd.searchTryId,
        searchRequestForDriverId = Just sd.id,
        clientId = clientId,
        driverId = driver.id,
        driverName = driver.firstName,
        driverRating = SP.roundToOneDecimal <$> driverStats.rating,
        status = DDrQuote.Active,
        vehicleVariant = sd.vehicleVariant,
        vehicleServiceTier = sd.vehicleServiceTier,
        distance = searchReq.estimatedDistance,
        distanceToPickup = sd.actualDistanceToPickup,
        durationToPickup = sd.durationToPickup,
        currency = sd.currency,
        distanceUnit = sd.distanceUnit,
        createdAt = now,
        updatedAt = now,
        validTill = addUTCTime driverQuoteExpirationSeconds now,
        providerId = searchReq.providerId,
        estimatedFare,
        fareParams,
        specialLocationTag = searchReq.specialLocationTag,
        specialLocationName = searchReq.specialLocationName,
        goHomeRequestId = sd.goHomeRequestId,
        tripCategory = tripCategory,
        estimateId = Id estimateId,
        clientSdkVersion = mbClientVersion',
        clientBundleVersion = mbBundleVersion',
        clientConfigVersion = mbConfigVersion',
        clientDevice = getDeviceFromText mbDevice',
        backendConfigVersion = Nothing,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        merchantOperatingCityId = Just searchReq.merchantOperatingCityId,
        vehicleServiceTierName = sd.vehicleServiceTierName,
        coinsRewardedOnGoldTierRide = sd.coinsRewardedOnGoldTierRide,
        reactBundleVersion = driver.reactBundleVersion <|> mbReactBundleVersion',
        commissionCharges = sd.commissionCharges,
        preferenceMatchScore = sd.preferenceMatchScore,
        isAutoAccepted = sd.isAutoAccepted
      }
