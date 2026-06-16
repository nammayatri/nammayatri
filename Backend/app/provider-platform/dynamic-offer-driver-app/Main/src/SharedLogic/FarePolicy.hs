{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SharedLogic.FarePolicy where

import BecknV2.OnDemand.Tags as Tags
import Control.Applicative ((<|>))
import Data.Aeson as A
import Data.Coerce (coerce)
import qualified Data.Geohash as Geohash
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Data.Text as T hiding (elem, find, map, null)
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar.WeekDate
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.Common
import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.FareParameters as DFareParameters
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds as DDriverExtraFeeBounds
import qualified Domain.Types.FareProduct as FareProduct
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleServiceTier as DVST
import Domain.Utils (mapConcurrently)
import GHC.Float (int2Double)
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Lib.Types.SpecialLocation as DSpecialLocation
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CallInternalMLPricing as ML
import SharedLogic.DynamicPricing
import qualified SharedLogic.FareCalculator as SFC
import qualified SharedLogic.FareProduct as FareProduct
import qualified SharedLogic.Merchant as SMerchant
import SharedLogic.Pricing
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.FarePolicy as QFP
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.CancellationFarePolicy as QCCFP
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Tools.DynamicLogic
import Tools.Error
import Tools.Maps
import Utils.Common.Cac.KeyNameConstants

data FarePoliciesProduct = FarePoliciesProduct
  { farePolicies :: [FarePolicyD.FullFarePolicy],
    area :: SL.Area,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    specialLocationSupportNumber :: Maybe Text,
    mbPickupDropArea :: Maybe SL.Area
  }

makeFarePolicyByEstOrQuoteIdKey :: Text -> Text
makeFarePolicyByEstOrQuoteIdKey estOrQuoteId = "CachedQueries:FarePolicy:EstOrQuoteId-" <> estOrQuoteId

getFarePolicyByEstOrQuoteIdWithoutFallback :: (CacheFlow m r) => Text -> m (Maybe FarePolicyD.FullFarePolicy)
getFarePolicyByEstOrQuoteIdWithoutFallback estOrQuoteId = do
  Hedis.runInMultiCloudRedisMaybeResult $
    Hedis.safeGet (makeFarePolicyByEstOrQuoteIdKey estOrQuoteId) >>= \case
      Nothing -> do
        logWarning $ "Fare Policy Not Found for quote id: " <> estOrQuoteId
        return Nothing
      Just a -> return $ Just $ coerce @(FarePolicyD.FullFarePolicyD 'Unsafe) @FarePolicyD.FullFarePolicy a

getFarePolicyByEstOrQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], ClickhouseFlow m r) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicyByEstOrQuoteId mbFromlocaton mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId mbBookingStartTime isDashboardRequest mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName =
  Hedis.runInMultiCloudRedisMaybeResult (Hedis.safeGet (makeFarePolicyByEstOrQuoteIdKey estOrQuoteId)) >>= \case
    Nothing -> do
      logWarning "Old Fare Policy Not Found, Hence using new fare policy."
      getFarePolicy mbFromlocaton mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId (fromMaybe False isDashboardRequest) tripCategory vehicleServiceTier area mbBookingStartTime mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName
    Just a -> return $ coerce @(FarePolicyD.FullFarePolicyD 'Unsafe) @FarePolicyD.FullFarePolicy a

cacheFarePolicyByQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> FarePolicyD.FullFarePolicy -> m ()
cacheFarePolicyByQuoteId quoteId fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeFarePolicyByEstOrQuoteIdKey quoteId) (coerce @FarePolicyD.FullFarePolicy @(FarePolicyD.FullFarePolicyD 'Unsafe) fp) expTime

-- 30 Mins, Assuming that all searchTries would be done by then. Correct logic would be searchRequestExpirationTime * searchRepeatLimit
cacheFarePolicyByEstimateId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> FarePolicyD.FullFarePolicy -> m ()
cacheFarePolicyByEstimateId estimateId fp = Hedis.setExp (makeFarePolicyByEstOrQuoteIdKey estimateId) (coerce @FarePolicyD.FullFarePolicy @(FarePolicyD.FullFarePolicyD 'Unsafe) fp) 1800

clearCachedFarePolicyByEstOrQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> m ()
clearCachedFarePolicyByEstOrQuoteId = Hedis.del . makeFarePolicyByEstOrQuoteIdKey

getFarePolicyOnEndRide :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], ClickhouseFlow m r) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> LatLong -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicyOnEndRide mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration toLocationLatLong merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId mbBookingStartTime isDashboardRequest mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName = do
  selectedFarePolicy <- case area of
    Just SL.Default -> handleFarePolicy
    Just (SL.Drop _) -> handleFarePolicy
    _ -> getFarePolicyByEstOrQuoteId mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId mbBookingStartTime isDashboardRequest mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName
  return selectedFarePolicy
  where
    handleFarePolicy = do
      dropSpecialLocation <- QSpecialLocation.findSpecialLocationByLatLong' toLocationLatLong >>= mapM (FareProduct.getDropSpecialLocation merchantOpCityId)
      let isDropSpecialLocation = (Just . SL.Drop . DSpecialLocation.id . fst) =<< dropSpecialLocation
          dropSpecialLocName = (.locationName) . fst <$> dropSpecialLocation
      selectedFarePolicy' <-
        if isJust isDropSpecialLocation && area /= isDropSpecialLocation
          then getFarePolicy mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId (fromMaybe False isDashboardRequest) tripCategory vehicleServiceTier isDropSpecialLocation mbBookingStartTime mbAppDynamicLogicVersion txnId configInExperimentVersions dropSpecialLocName
          else getFarePolicyByEstOrQuoteId mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId mbBookingStartTime isDashboardRequest mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName
      logInfo $ "Drop Special Location during end ride: " <> show isDropSpecialLocation <> " and area at estimate stage is: " <> show area <> ", fare policyId for end ride calc: " <> show (selectedFarePolicy'.id)
      return selectedFarePolicy'

getFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], ClickhouseFlow m r) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> Bool -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Maybe UTCTime -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicy mbFromlocaton mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId isDashboard tripCategory serviceTier mbArea mbBookingStartTime mbAppDynamicLogicVersion txnId configsInExperimentVersions mbSpecialLocName = do
  case mbArea of
    Nothing -> do
      fareProduct <- getFareProduct' SL.Default serviceTier >>= fromMaybeM NoFareProduct
      getFarePolicyWithArea SL.Default fareProduct
    Just area -> do
      logInfo $ "Dynamic Pricing debugging getFarePolicy txnId: " <> show txnId <> " and area : " <> show area
      mbFareProduct <- getFareProduct' area serviceTier
      logInfo $ "Dynamic Pricing debugging getFarePolicy txnId: " <> show txnId <> " and mbFareProduct : " <> show mbFareProduct
      case mbFareProduct of
        Just fareProduct -> do
          fp <- getFarePolicyWithArea area fareProduct
          logInfo $ "Dynamic Pricing debugging getFarePolicy txnId: " <> show txnId <> " and getFarePolicyWithArea : " <> show fp
          return fp
        Nothing -> do
          -- Gate ID fallback: try without gate ID before falling to Default
          let baseArea = SL.stripGateId area
          mbBaseProduct <- if SL.hasGateId area then getFareProduct' baseArea serviceTier else pure Nothing
          case mbBaseProduct of
            Just fareProduct -> do
              logInfo $ "Dynamic Pricing debugging getFarePolicy txnId: " <> show txnId <> " and gate fallback to baseArea : " <> show baseArea
              getFarePolicyWithArea baseArea fareProduct
            Nothing -> do
              fareProduct <- getFareProduct' SL.Default serviceTier >>= fromMaybeM NoFareProduct
              logInfo $ "Dynamic Pricing debugging getFarePolicy txnId: " <> show txnId <> " and getFareProduct' : " <> show fareProduct
              fp <- getFarePolicyWithArea SL.Default fareProduct
              logInfo $ "Dynamic Pricing debugging getFarePolicy txnId: " <> show txnId <> " and getFarePolicyWithArea Nothing' : " <> show fp
              return fp
  where
    getFareProduct' areaName serviceTierName = do
      FareProduct.getBoundedFareProduct merchantOpCityId searchSources tripCategory serviceTierName areaName
        |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTierName areaName
    searchSources = FareProduct.getSearchSources isDashboard
    getFarePolicyWithArea areaName fareProduct = do
      mbVehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow serviceTier merchantOpCityId configsInExperimentVersions (SL.pickupSpecialZoneIdFromArea areaName)
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and findByServiceTierTypeAndCityId : " <> show mbVehicleServiceTierItem
      (mbBaseVariantCarFareProduct :: Maybe FareProduct.FareProduct) <- case mbVehicleServiceTierItem of
        Just vehicleServiceTierItem -> do
          if vehicleServiceTierItem.vehicleCategory == Just DVC.CAR
            then do
              maybe (return Nothing) (\vehicleServiceTier -> getFareProduct' areaName vehicleServiceTier.serviceTierType)
                =<< CQVST.findBaseServiceTierTypeByCategoryAndCityIdInRideFlow (Just DVC.CAR) merchantOpCityId configsInExperimentVersions (SL.pickupSpecialZoneIdFromArea areaName)
            else return Nothing
        Nothing -> return Nothing
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and mbBaseVariantCarFareProduct : " <> show mbBaseVariantCarFareProduct
      baseVariantFareAmountCar <- getBaseVariantFarePolicy mbFromlocaton mbFromlocaton merchantOpCityId mbBaseVariantCarFareProduct txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion configsInExperimentVersions mbSpecialLocName (SL.pickupSpecialZoneIdFromArea areaName) []
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and baseVariantFareAmountCar : " <> show baseVariantFareAmountCar
      fp <- getFullFarePolicy mbFromlocaton mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId mbBookingStartTime baseVariantFareAmountCar mbAppDynamicLogicVersion mbSpecialLocName (SL.pickupSpecialZoneIdFromArea areaName) fareProduct configsInExperimentVersions [] Nothing (Just mbVehicleServiceTierItem) >>= fromMaybeM NoFarePolicy
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and getFullFarePolicy : " <> show fp
      return fp

-- Dynamic-pricing congestion/QAR is only computed for on-demand one-way trips
-- (mirrors the 'DTC.OneWay v | v /= MeterRide' guard in 'getFullFarePolicy').
isDynamicPricingTripCategory :: DTC.TripCategory -> Bool
isDynamicPricingTripCategory (DTC.OneWay v) = v /= MeterRide
isDynamicPricingTripCategory _ = False

getAllFarePoliciesProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], ClickhouseFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> LatLong -> Maybe LatLong -> Maybe (Id SL.SpecialLocation) -> Maybe (Id SL.SpecialLocation) -> Maybe CacKey -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Maybe Int -> DTC.TripCategory -> [LYT.ConfigVersionMap] -> m FarePoliciesProduct
getAllFarePoliciesProduct merchantId merchantOpCityId isDashboard fromlocaton mbToLocation mbFromSpecialLocationId mbToSpecialLocationId txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion tripCategory configsInExperimentVersions = do
  let searchSources = FareProduct.getSearchSources isDashboard
  allFareProducts <- FareProduct.getAllFareProducts merchantId merchantOpCityId searchSources fromlocaton mbToLocation mbFromSpecialLocationId mbToSpecialLocationId tripCategory
  (mbBaseVariantCarFareProduct :: Maybe FareProduct.FareProduct) <-
    return . getFareProduct allFareProducts
      =<< CQVST.findBaseServiceTierTypeByCategoryAndCityIdInRideFlow (Just DVC.CAR) merchantOpCityId configsInExperimentVersions ((.getId) <$> mbFromSpecialLocationId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId txnId)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  -- Resolve each fareProduct's vehicle-service-tier item once and reuse it for
  -- both the per-vehicleCategory dynamic-pricing inputs and inside
  -- 'getFullFarePolicy', so the (cached) lookup isn't repeated per service tier.
  resolvedFareProducts <- forM allFareProducts.fareProducts $ \fareProduct -> (fareProduct,) <$> CQVST.findByServiceTierTypeAndCityIdInRideFlow fareProduct.vehicleServiceTier merchantOpCityId configsInExperimentVersions ((.getId) <$> mbFromSpecialLocationId)
  -- Pre-resolve the dynamic-pricing Redis inputs once for the whole search: the
  -- congestion/rain/toss part is shared and the QAR/supply-demand part is fetched
  -- per distinct vehicleCategory, so service tiers sharing a category don't
  -- re-hit Redis. Skipped (falls back to per-tier fetch) when dynamic pricing
  -- isn't applicable for this search.
  dpInputsList <-
    case (transporterConfig.isDynamicPricingQARCalEnabled == Just True && isDynamicPricingTripCategory tripCategory, mbDistance) of
      (True, Just distance) -> do
        now <- getCurrentTime
        let geohash = fromMaybe (fromMaybe "" $ T.pack <$> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (fromlocaton.lat, fromlocaton.lon)) mbFromLocGeohash
            vehicleCategories = map (\(_, mbItem) -> maybe Nothing (.vehicleCategory) mbItem) resolvedFareProducts
            qarRadius = fromMaybe 5.0 transporterConfig.qarCalRadiusInKm
        buildDynamicPricingInputs now fromlocaton qarRadius geohash mbToLocGeohash distance.getMeters merchantOpCityId.getId vehicleCategories
      _ -> pure []
  baseVariantFareAmountCar <- getBaseVariantFarePolicy (Just fromlocaton) mbToLocation merchantOpCityId mbBaseVariantCarFareProduct txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion configsInExperimentVersions allFareProducts.specialLocationName ((.getId) <$> mbFromSpecialLocationId) dpInputsList
  farePolicies <- catMaybes <$> mapConcurrently (\(fareProduct, mbVehicleServiceTierItem) -> getFullFarePolicy (Just fromlocaton) mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId Nothing baseVariantFareAmountCar mbAppDynamicLogicVersion allFareProducts.specialLocationName ((.getId) <$> mbFromSpecialLocationId) fareProduct configsInExperimentVersions dpInputsList (Just transporterConfig) (Just mbVehicleServiceTierItem)) resolvedFareProducts
  return $
    FarePoliciesProduct
      { farePolicies,
        area = allFareProducts.area,
        specialLocationTag = allFareProducts.specialLocationTag,
        specialLocationName = allFareProducts.specialLocationName,
        specialLocationSupportNumber = allFareProducts.specialLocationSupportNumber,
        mbPickupDropArea = allFareProducts.mbPickupDropArea
      }

getBaseVariantFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], ClickhouseFlow m r) => Maybe LatLong -> Maybe LatLong -> Id DMOC.MerchantOperatingCity -> Maybe FareProduct.FareProduct -> Maybe CacKey -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Maybe Int -> [LYT.ConfigVersionMap] -> Maybe Text -> Maybe Text -> [(Maybe DVC.VehicleCategory, DynamicPricingInputs)] -> m (Maybe HighPrecMoney)
getBaseVariantFarePolicy mbFromLocation mbToLocation merchantOpCityId mbBaseVariantCarFareProduct txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion configsInExperimentVersions mbSpecialLocName mbSpecialZoneId dpInputsList = do
  mbBaseVariantFarePolicy <- join <$> traverse (\fareProduct -> getFullFarePolicy mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId Nothing Nothing mbAppDynamicLogicVersion mbSpecialLocName mbSpecialZoneId fareProduct configsInExperimentVersions dpInputsList Nothing Nothing) mbBaseVariantCarFareProduct
  case mbBaseVariantFarePolicy of
    Just baseVariantFullFarePolicy -> do
      parameters <- calculateFareParametersForFarePolicy baseVariantFullFarePolicy mbDistance mbDuration merchantOpCityId
      let estimatedFare = SFC.fareSum parameters (Just [])
      -- estimatedFare <- calculateFareForFarePolicy baseVariantFullFarePolicy mbDistance mbDuration merchantOpCityId
      return $ Just estimatedFare
    Nothing -> return Nothing

getFareProduct :: FareProduct.FareProducts -> Maybe DVST.VehicleServiceTier -> Maybe FareProduct.FareProduct
getFareProduct _ Nothing = Nothing
getFareProduct fareProducts (Just vehicleServiceTier) = List.find (\fareProduct -> fareProduct.vehicleServiceTier == vehicleServiceTier.serviceTierType) fareProducts.fareProducts

getFullFarePolicy :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], ClickhouseFlow m r) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Maybe CacKey -> Maybe UTCTime -> Maybe HighPrecMoney -> Maybe Int -> Maybe Text -> Maybe Text -> FareProduct.FareProduct -> [LYT.ConfigVersionMap] -> [(Maybe DVC.VehicleCategory, DynamicPricingInputs)] -> Maybe TransporterConfig -> Maybe (Maybe DVST.VehicleServiceTier) -> m (Maybe FarePolicyD.FullFarePolicy)
getFullFarePolicy mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId mbBookingStartTime mbBaseVaraintCarPrice mbAppDynamicLogicVersion mbSpecialLocName mbSpecialZoneId fareProduct configsInExperimentVersions dpInputsList mbTransporterConfig mbPreResolvedVSTItem = do
  transporterConfig <- maybe (getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = fareProduct.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId fareProduct.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound fareProduct.merchantOperatingCityId.getId)) pure mbTransporterConfig
  mbVehicleServiceTierItem <-
    maybe (CQVST.findByServiceTierTypeAndCityIdInRideFlow fareProduct.vehicleServiceTier fareProduct.merchantOperatingCityId configsInExperimentVersions mbSpecialZoneId) pure mbPreResolvedVSTItem
  let whiteListedGeohashes = fromMaybe [] transporterConfig.dpWhiteListedGeohash
      blackListedGeohashes = fromMaybe [] transporterConfig.dpBlackListedGeohash
  now <- getCurrentTime
  let _bookingStartTime = fromMaybe now mbBookingStartTime
  let localTimeZoneSeconds = transporterConfig.timeDiffFromUtc
  congestionChargeStartTime <- getCurrentTime
  congestionChargeMultiplierFromModel <-
    case fareProduct.tripCategory of
      DTC.OneWay v | v /= MeterRide -> do
        maybe (return Nothing) (checkGeoHashAndCalculate mbVehicleServiceTierItem localTimeZoneSeconds whiteListedGeohashes blackListedGeohashes transporterConfig mbFromLocGeohash) mbFromLocation
      DTC.CrossCity v _ | v /= MeterRide -> do
        maybe (return Nothing) (checkGeoHashAndCalculate mbVehicleServiceTierItem localTimeZoneSeconds whiteListedGeohashes blackListedGeohashes transporterConfig mbFromLocGeohash) mbFromLocation
      _ -> return Nothing
  congestionChargeEndTime <- getCurrentTime
  let congestionChargeDuration = diffUTCTime congestionChargeEndTime congestionChargeStartTime
  logInfo $ "getFullFarePolicy: calculateCongestionChargeViaML took " <> show congestionChargeDuration
  mbFarePolicy' <- QFP.findById txnId fareProduct.farePolicyId
  case mbFarePolicy' of
    Nothing -> do
      logError $ "No fare policy found for farePolicyId: " <> show fareProduct.farePolicyId
      return Nothing
    Just farePolicy' -> do
      cancellationFarePolicy <- maybe (return Nothing) QCCFP.findById farePolicy'.cancellationFarePolicyId
      let getMlCongestionChargeMultiplier =
            if transporterConfig.isMLBasedDynamicPricingEnabled
              then calculateCongestionChargeViaML mbDistance mbFromLocation cancellationFarePolicy farePolicy' Nothing
              else return Nothing
      (updatedCongestionChargePerMin, updatedCongestionChargeMultiplier, version, supplyDemandRatioFromLoc, supplyDemandRatioToLoc, smartTipSuggestion, smartTipReason, mbActualQARFromLocGeohash, mbActualQARCity, mbcongestionChargeData, mbDriverExtraFeeBounds) <-
        case congestionChargeMultiplierFromModel of
          Just details ->
            case (details.congestionChargePerMin, details.smartTipSuggestion, details.congestionChargeMultiplier) of
              (Just congestionChargePerMinute, smartTip, Nothing) -> pure (Just congestionChargePerMinute, Nothing, details.dpVersion, details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, smartTip, details.smartTipReason, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds) -----------Need to send Nothing here for congestionChargeMultiplier
              (Nothing, smartTip, Just congestionChargeMultiplier) -> do
                mlCongestionChargeMultiplier <- getMlCongestionChargeMultiplier
                pure (Nothing, mlCongestionChargeMultiplier <|> Just congestionChargeMultiplier, if isJust mlCongestionChargeMultiplier then Just "ML" else details.dpVersion, details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, smartTip, details.smartTipReason, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds) -----------Need to send Nothing here for congestionChargePerMinute
              (Nothing, Just smartTip, Nothing) -> do
                mlCongestionChargeMultiplier <- getMlCongestionChargeMultiplier
                pure (Nothing, mlCongestionChargeMultiplier <|> farePolicy'.congestionChargeMultiplier, if isJust mlCongestionChargeMultiplier then Just "ML" else details.dpVersion, details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, Just smartTip, details.smartTipReason, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds)
              _ -> do
                mlCongestionChargeMultiplier <- getMlCongestionChargeMultiplier
                pure (Nothing, mlCongestionChargeMultiplier <|> farePolicy'.congestionChargeMultiplier, if isJust mlCongestionChargeMultiplier then Just "ML" else Just "Static", details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, Nothing, Nothing, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds)
          Nothing -> pure (Nothing, farePolicy'.congestionChargeMultiplier, Just "Static", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      let farePolicy = updateCongestionChargeMultiplier farePolicy' updatedCongestionChargeMultiplier mbDriverExtraFeeBounds
      logDebug $ "farePolicy after updating driverExtraFeeBounds: " <> show farePolicy <> " and mbDriverExtraFeeBounds: " <> show mbDriverExtraFeeBounds
      let congestionChargeDetails = FarePolicyD.CongestionChargeDetails version supplyDemandRatioToLoc supplyDemandRatioFromLoc updatedCongestionChargePerMin smartTipSuggestion smartTipReason mbActualQARFromLocGeohash mbActualQARCity
      let fullFarePolicy = FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleServiceTier fareProduct.tripCategory cancellationFarePolicy congestionChargeDetails mbcongestionChargeData farePolicy fareProduct.disableRecompute
      case mbVehicleServiceTierItem of
        Just vehicleServiceTierItem -> do
          if vehicleServiceTierItem.vehicleCategory == Just DVC.CAR && isJust mbBaseVaraintCarPrice && not (fromMaybe True vehicleServiceTierItem.baseVehicleServiceTier)
            then do
              let addtionalFarePerKm = toRational $ fromMaybe 0 vehicleServiceTierItem.fareAdditionPerKmOverBaseServiceTier
              let mbAdditionalFare = mbDistance <&> \distance -> toHighPrecMoney ((fromIntegral distance.getMeters / 1000 :: Rational) * addtionalFarePerKm)
              let fareWithAddition = fromMaybe 0.0 mbBaseVaraintCarPrice + fromMaybe 0.0 mbAdditionalFare
              parameters <- calculateFareParametersForFarePolicy fullFarePolicy mbDistance mbDuration fareProduct.merchantOperatingCityId
              let estimatedFare = SFC.fareSum parameters (Just [])
              if fareWithAddition > estimatedFare
                then do
                  logWarning $ "Fare with addition: " <> show fareWithAddition <> " is greater than estimated fare: " <> show estimatedFare
                  let fullFarePolicy' = fullFarePolicy{additionalCongestionCharge = fareWithAddition - estimatedFare}
                  return $ Just fullFarePolicy'
                else return $ Just fullFarePolicy
            else return $ Just fullFarePolicy
        Nothing -> return $ Just fullFarePolicy
  where
    checkGeoHashAndCalculate mbVehicleServiceTierItem localTimeZoneSeconds whiteListedGeohashes blackListedGeohashes transporterConfig mbfromLocGeohash fromLocation = do
      let fromLocGeohash = fromMaybe (fromMaybe "" $ T.pack <$> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (fromLocation.lat, fromLocation.lon)) mbfromLocGeohash
      if elem fromLocGeohash whiteListedGeohashes || notElem fromLocGeohash blackListedGeohashes
        then do
          logInfo $ "Calling DynamicPricing 1" <> show localTimeZoneSeconds <> show fromLocGeohash <> show mbToLocGeohash <> show fareProduct.vehicleServiceTier <> show mbDistance <> show mbDuration <> show transporterConfig.isDynamicPricingQARCalEnabled <> show transporterConfig.qarCalRadiusInKm <> show mbAppDynamicLogicVersion <> show fareProduct.merchantOperatingCityId
          let vehicleCategory = maybe Nothing (.vehicleCategory) mbVehicleServiceTierItem
              mbDpInputs = lookup vehicleCategory dpInputsList
          getCongestionChargeMultiplierFromModel' mbDpInputs localTimeZoneSeconds (Just fromLocation) (Just fromLocGeohash) mbToLocGeohash fareProduct.vehicleServiceTier vehicleCategory mbDistance mbDuration transporterConfig.isDynamicPricingQARCalEnabled transporterConfig.qarCalRadiusInKm mbSpecialLocName mbAppDynamicLogicVersion fareProduct.merchantOperatingCityId mbDuration Nothing
        else return Nothing
    -- calculateCongestionChargeViaML :: ( MonadFlow m,
    --     CacheFlow m r,
    --     EsqDBFlow m r,
    --     EsqDBReplicaFlow m r
    --   ) =>
    --   m (Maybe FarePolicyD.CongestionChargeMultiplier,  Maybe Double)
    calculateCongestionChargeViaML (Just distance) (Just fromLocation) cancellationFarePolicy farePolicy mbcongestionChargeData = do
      let congestionChargeDetails =
            FarePolicyD.CongestionChargeDetails
              { dpVersion = Nothing,
                mbSupplyDemandRatioToLoc = Nothing,
                mbSupplyDemandRatioFromLoc = Nothing,
                congestionChargePerMin = Nothing,
                smartTipSuggestion = Nothing,
                smartTipReason = Nothing,
                mbActualQARFromLocGeohash = Nothing,
                mbActualQARCity = Nothing
              }
      let fullFarePolicy = FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleServiceTier fareProduct.tripCategory cancellationFarePolicy congestionChargeDetails mbcongestionChargeData farePolicy fareProduct.disableRecompute
      parameters <- calculateFareParametersForFarePolicy fullFarePolicy (Just distance) mbDuration fareProduct.merchantOperatingCityId
      let fare = SFC.fareSum parameters (Just [])
      case parameters.fareParametersDetails of
        DFareParameters.ProgressiveDetails DFareParameters.FParamsProgressiveDetails {deadKmFare, extraKmFare} -> do
          let baseFare = parameters.baseFare
              distanceFare = extraKmFare
              pickupFare = deadKmFare
              maxDAFare = case fullFarePolicy.driverExtraFeeBounds of
                Just nonEmptydriverExtraFeeBounds -> do
                  Just (DDriverExtraFeeBounds.findDriverExtraFeeBoundsByDistance distance nonEmptydriverExtraFeeBounds).maxFee
                Nothing -> Nothing
          mlPricingInternal <- asks (.mlPricingInternal)
          let req =
                ML.GetCongestionChargeReq
                  { txnId = (Just . getKeyValue) =<< txnId,
                    fromLocation = fromLocation,
                    toLocation = mbToLocation,
                    distance = distance,
                    duration = mbDuration,
                    bookingTime = mbBookingStartTime,
                    merchantOperatingCityId = fareProduct.merchantOperatingCityId,
                    serviceTier = fareProduct.vehicleServiceTier,
                    fare = Just fare,
                    ..
                  }
          -- If ML call fails for any reason, fall back to no congestion multiplier to keep flow running
          resOrErr <- withTryCatch "getCongestionCharge:getFullFarePolicy" (ML.getCongestionCharge mlPricingInternal.apiKey mlPricingInternal.url req)
          case resOrErr of
            Right congestionChargeRes -> return congestionChargeRes.congestionChargeMultiplier
            Left e -> do
              logError $ "getCongestionCharge via MLfailed, defaulting to Nothing: " <> show e <> "   :: req body is " <> show req
              return Nothing
        -- return fullFarePolicy.congestionChargeMultiplier
        _ -> return Nothing
    calculateCongestionChargeViaML _ _ _ _ _ = return Nothing

updateCongestionChargeMultiplier :: FarePolicyD.FarePolicy -> Maybe FarePolicyD.CongestionChargeMultiplier -> Maybe DDriverExtraFeeBounds.DriverExtraFeeBounds -> FarePolicyD.FarePolicy
updateCongestionChargeMultiplier FarePolicyD.FarePolicy {..} congestionMultiplier driverExtraFeeBounds' = FarePolicyD.FarePolicy {congestionChargeMultiplier = congestionMultiplier, driverExtraFeeBounds = maybe driverExtraFeeBounds (NE.nonEmpty . pure) driverExtraFeeBounds', ..}

calculateFareParametersForFarePolicy :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, BeamFlow m r) => FarePolicyD.FullFarePolicy -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> m DFareParameters.FareParameters
calculateFareParametersForFarePolicy fullFarePolicy mbDistance mbDuration merchantOperatingCityId = do
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOperatingCityId
  distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOperatingCityId
  now <- getCurrentTime
  mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing))
  let gstBreakup = mbTransporterConfig <&> (.taxConfig.rideGst)
  let params =
        SFC.CalculateFareParametersParams
          { farePolicy = fullFarePolicy,
            actualDistance = mbDistance,
            rideTime = now, ----------assumption as this logic should work only for one way book now trips
            returnTime = Nothing,
            roundTrip = False,
            waitingTime = Nothing,
            stopWaitingTimes = [],
            actualRideDuration = Nothing,
            vehicleAge = Nothing,
            driverSelectedFare = Nothing,
            customerExtraFee = Nothing,
            nightShiftCharge = Nothing,
            customerCancellationDues = Nothing,
            nightShiftOverlapChecking = False, ---------considered only for one way
            estimatedDistance = mbDistance,
            estimatedRideDuration = mbDuration,
            estimatedCongestionCharge = Nothing,
            timeDiffFromUtc = Nothing,
            petCharges = Nothing, ----------check
            shouldApplyBusinessDiscount = False,
            shouldApplyPersonalDiscount = True,
            tollCharges = Nothing, ------fix it in future
            noOfStops = 0, ------fix it in future
            currency,
            distanceUnit,
            merchantOperatingCityId = Just merchantOperatingCityId,
            mbAdditonalChargeCategories = Nothing,
            numberOfLuggages = Nothing,
            govtChargesRate = gstBreakup,
            pickupGateId = Nothing
          }
  SFC.calculateFareParameters params

mkFarePolicyBreakups :: (Text -> breakupItemValue) -> (Text -> breakupItemValue -> breakupItem) -> Maybe Meters -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> Maybe Double -> FarePolicyD.FarePolicy -> [breakupItem]
mkFarePolicyBreakups mkValue mkBreakupItem mbDistance mbCancellationCharge mbTollCharges estimatedTotalFare congestionChargeViaDp mbGovtChargesRate farePolicy = do
  let distance = fromMaybe 0 mbDistance -- TODO: Fix Later
      driverExtraFeeBounds = FarePolicyD.findDriverExtraFeeBoundsByDistance distance <$> farePolicy.driverExtraFeeBounds
      nightShiftBounds = farePolicy.nightShiftBounds

      tollChargesCaption = show Tags.TOLL_CHARGES
      tollChargesItem = mkBreakupItem tollChargesCaption . (mkValue . show) <$> mbTollCharges

      cancellationChargeCaption = show Tags.CANCELLATION_CHARGES
      cancellationChargeItem = mkBreakupItem cancellationChargeCaption . (mkValue . highPrecMoneyToText) <$> mbCancellationCharge

      congestionChargePercentageCaption = show Tags.CONGESTION_CHARGE_PERCENTAGE
      congestionChargePercentageItemMultiplier =
        farePolicy.congestionChargeMultiplier <&> \case
          FarePolicyD.BaseFareAndExtraDistanceFare congestionChargeMultiplier -> mkBreakupItem congestionChargePercentageCaption (mkValue $ show ((congestionChargeMultiplier - 1) * 100))
          FarePolicyD.ExtraDistanceFare congestionChargeMultiplier -> mkBreakupItem congestionChargePercentageCaption (mkValue $ show ((congestionChargeMultiplier - 1) * 100))
      congestionChargePercentageItemDp =
        congestionChargeViaDp <&> \congestionCharge -> mkBreakupItem congestionChargePercentageCaption (mkValue $ show (congestionCharge / estimatedTotalFare * 100))
      congestionChargePercentageItem = congestionChargePercentageItemDp <|> congestionChargePercentageItemMultiplier
      parkingChargeCaption = show Tags.PARKING_CHARGE
      parkingChargeItem = mkBreakupItem parkingChargeCaption . (mkValue . show) <$> farePolicy.parkingCharge

      perStopChargesCaption = show Tags.PER_STOP_CHARGES
      perStopChargeItem = mkBreakupItem perStopChargesCaption . (mkValue . show) <$> farePolicy.perStopCharge

      petChargesCaption = show Tags.PET_CHARGES
      petChargesItem = mkBreakupItem petChargesCaption . (mkValue . show) <$> farePolicy.petCharges

      driverAllowanceCaption = show Tags.DRIVER_ALLOWANCE
      driverAllowanceItem = mkBreakupItem driverAllowanceCaption . (mkValue . show) <$> farePolicy.driverAllowance

      airportConvenienceFeeCaption = show Tags.AIRPORT_CONVENIENCE_FEE
      airportConvenienceFeeItem = mkBreakupItem airportConvenienceFeeCaption . (mkValue . show) <$> farePolicy.airportConvenienceFee

      businessDiscountCaption = show Tags.BUSINESS_DISCOUNT_PERCENTAGE
      businessDiscountItem = mkBreakupItem businessDiscountCaption . (mkValue . show) <$> farePolicy.businessDiscountPercentage

      personalDiscountCaption = show Tags.PERSONAL_DISCOUNT_PERCENTAGE
      personalDiscountItem = mkBreakupItem personalDiscountCaption . (mkValue . show) <$> farePolicy.personalDiscountPercentage

      priorityChargesCaption = show Tags.PRIORITY_CHARGES
      priorityChargesItem = mkBreakupItem priorityChargesCaption . (mkValue . show) <$> farePolicy.priorityCharges

      serviceChargeCaption = show Tags.SERVICE_CHARGE
      serviceChargeItem = mkBreakupItem serviceChargeCaption . (mkValue . highPrecMoneyToText) <$> farePolicy.serviceCharge

      luggageChargeCaption = show Tags.LUGGAGE_CHARGE
      luggageChargeItem = mkBreakupItem luggageChargeCaption . (mkValue . show) <$> farePolicy.perLuggageCharge

      (boothChargeFixedItem, boothChargePercentageItem) =
        case farePolicy.boothCharges of
          Just (FarePolicyD.BoothChargeFixed hpm) ->
            (Just $ mkBreakupItem (show Tags.BOOTH_CHARGE) (mkValue $ highPrecMoneyToText hpm), Nothing)
          Just (FarePolicyD.BoothChargePercentage d) ->
            (Nothing, Just $ mkBreakupItem (show Tags.BOOTH_CHARGE_PERCENTAGE) (mkValue $ show d))
          Nothing -> (Nothing, Nothing)

      (returnFeeFixedItem, returnFeePercentageItem) =
        case farePolicy.returnFee of
          Just (FarePolicyD.ReturnFeeFixed hpm) ->
            (Just $ mkBreakupItem (show Tags.RETURN_FEE) (mkValue $ highPrecMoneyToText hpm), Nothing)
          Just (FarePolicyD.ReturnFeePercentage d) ->
            (Nothing, Just $ mkBreakupItem (show Tags.RETURN_FEE_PERCENTAGE) (mkValue $ show d))
          Nothing -> (Nothing, Nothing)

      governmentChargeCaption = show Tags.GOVERNMENT_CHARGE
      governmentChargeItem = mkBreakupItem governmentChargeCaption . (mkValue . show) <$> mbGovtChargesRate

      mbDriverExtraFeeBoundSections = farePolicy.driverExtraFeeBounds <&> \driverExtraFeeBound -> NE.sortBy (comparing (.startDistance)) driverExtraFeeBound
      driverExtraFeeBoundsMinFeeItems = maybe [] (\driverExtraFeeBoundSections -> mkDriverExtraFeeBoundsMinFeeItem [] (toList driverExtraFeeBoundSections) 0) mbDriverExtraFeeBoundSections
      driverExtraFeeBoundsMaxFeeItems = maybe [] (\driverExtraFeeBoundSections -> mkDriverExtraFeeBoundsMaxFeeItem [] (toList driverExtraFeeBoundSections) 0) mbDriverExtraFeeBoundSections

      driverMinExtraFee = driverExtraFeeBounds <&> (.minFee)
      driverMinExtraFeeCaption = show Tags.DRIVER_MIN_EXTRA_FEE
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption . (mkValue . highPrecMoneyToText) <$> driverMinExtraFee

      driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee)
      driverMaxExtraFeeCaption = show Tags.DRIVER_MAX_EXTRA_FEE
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption . (mkValue . highPrecMoneyToText) <$> driverMaxExtraFee

      nightShiftStart = nightShiftBounds <&> (.nightShiftStart)
      nightShiftStartInSecondsCaption = show Tags.NIGHT_SHIFT_START_TIME_IN_SECONDS
      nightShiftStartInSecondsItem = mkBreakupItem nightShiftStartInSecondsCaption . (mkValue . show . (.getSeconds) . secondsFromTimeOfDay) <$> nightShiftStart

      nightShiftEnd = nightShiftBounds <&> (.nightShiftEnd)
      nightShiftEndInSecondsCaption = show Tags.NIGHT_SHIFT_END_TIME_IN_SECONDS
      nightShiftEndInSecondsItem = mkBreakupItem nightShiftEndInSecondsCaption . (mkValue . show . (.getSeconds) . secondsFromTimeOfDay) <$> nightShiftEnd

      nightShiftStartCaption = show Tags.NIGHT_SHIFT_START_TIME
      nightShiftStartItem = mkBreakupItem nightShiftStartCaption . (mkValue . show) <$> nightShiftStart

      nightShiftEndCaption = show Tags.NIGHT_SHIFT_END_TIME
      nightShiftEndItem = mkBreakupItem nightShiftEndCaption . (mkValue . show) <$> nightShiftEnd

      insuranceChargeCaption = mkInsuranceChargeCaption farePolicy.distanceUnit
      insuranceChargeItem = mkBreakupItem insuranceChargeCaption . (mkValue . highPrecMoneyToText) <$> farePolicy.perDistanceUnitInsuranceCharge

      cardChargePercentageCaption = show Tags.CARD_CHARGE_PERCENTAGE
      cardChargePercentageItem = mkBreakupItem cardChargePercentageCaption . (mkValue . show . toPercentage) <$> (farePolicy.cardCharge >>= (.perDistanceUnitMultiplier))

      fixedCardChargeCaption = show Tags.FIXED_CARD_CHARGE
      fixedCardChargeItem = mkBreakupItem fixedCardChargeCaption . (mkValue . highPrecMoneyToText) <$> (farePolicy.cardCharge >>= (.fixed))

      customerExtraFeeTaxItem = do
        config <- farePolicy.vatChargeConfig
        guard (FarePolicyD.CustomerExtraFeeComponent `elem` config.appliesOn)
        case SFC.parseCodeValue config.value of
          Just (SFC.ParsedPercentage pct) ->
            Just $ mkBreakupItem (show Tags.CUSTOMER_EXTRA_FEE_TAX_MULTIPLIER) (mkValue $ show (1 + fromRational pct :: Double))
          Just (SFC.ParsedFixed amount) ->
            Just $ mkBreakupItem (show Tags.CUSTOMER_EXTRA_FEE_TAX_FIXED) (mkValue $ highPrecMoneyToText amount)
          Nothing -> Nothing

      additionalDetailsBreakups = processAdditionalDetails farePolicy.farePolicyDetails
      conditionalCharges = processAdditionalCharges farePolicy.conditionalCharges
  catMaybes
    [ tollChargesItem,
      serviceChargeItem,
      cancellationChargeItem,
      parkingChargeItem,
      driverMinExtraFeeItem,
      businessDiscountItem,
      personalDiscountItem,
      driverMaxExtraFeeItem,
      petChargesItem,
      driverAllowanceItem,
      airportConvenienceFeeItem,
      priorityChargesItem,
      nightShiftStartItem,
      nightShiftEndItem,
      nightShiftStartInSecondsItem,
      nightShiftEndInSecondsItem,
      congestionChargePercentageItem,
      insuranceChargeItem,
      cardChargePercentageItem,
      fixedCardChargeItem,
      perStopChargeItem,
      luggageChargeItem,
      boothChargeFixedItem,
      boothChargePercentageItem,
      returnFeeFixedItem,
      returnFeePercentageItem,
      governmentChargeItem,
      customerExtraFeeTaxItem
    ]
    <> additionalDetailsBreakups
    <> driverExtraFeeBoundsMinFeeItems
    <> driverExtraFeeBoundsMaxFeeItems
    <> conditionalCharges
  where
    mkInsuranceChargeCaption = \case
      Meter -> show Tags.INSURANCE_CHARGE_PER_METER
      Mile -> show Tags.INSURANCE_CHARGE_PER_MILE
      Kilometer -> show Tags.INSURANCE_CHARGE_PER_KM
      Yard -> show Tags.INSURANCE_CHARGE_PER_YARD

    toPercentage x = (x - 1) * 100

    processAdditionalCharges conditionalCharges = do
      List.map (\addCharges -> mkBreakupItem (show $ castAdditionalChargeCategoriesToTag addCharges.chargeCategory) . mkValue $ show addCharges.charge) conditionalCharges

    castAdditionalChargeCategoriesToTag = \case
      DAC.SAFETY_PLUS_CHARGES -> Tags.SAFETY_PLUS_CHARGES
      DAC.NO_CHARGES -> Tags.NO_CHARGES
      DAC.NYREGULAR_SUBSCRIPTION_CHARGE -> Tags.NYREGULAR_SUBSCRIPTION_CHARGE
    processAdditionalDetails = \case
      FarePolicyD.ProgressiveDetails det -> mkAdditionalProgressiveBreakups det
      FarePolicyD.SlabsDetails det -> mkAdditionalSlabBreakups $ FarePolicyD.findFPSlabsDetailsSlabByDistance (fromMaybe 0 mbDistance) det.slabs
      FarePolicyD.RentalDetails det -> mkAdditionalRentalBreakups det
      FarePolicyD.InterCityDetails det -> mkAdditionalInterCityBreakups det
      FarePolicyD.AmbulanceDetails det -> mkAdditionalAmbulanceBreakups det
    mkAdditionalRentalBreakups det = do
      let minFareCaption = show Tags.MIN_FARE
          minFareItem = mkBreakupItem minFareCaption . mkValue $ show det.baseFare

          perHourChargeCaption = show Tags.PER_HOUR_CHARGE
          perHourChargeItem = mkBreakupItem perHourChargeCaption . mkValue $ show det.perHourCharge

          perExtraMinRateCaption = show Tags.PER_MINUTE_CHARGE
          perExtraMinRateItem = mkBreakupItem perExtraMinRateCaption . mkValue $ show det.perExtraMinRate

          perExtraKmRateCaption = show Tags.UNPLANNED_PER_KM_CHARGE
          perExtraKmRateItem = mkBreakupItem perExtraKmRateCaption . mkValue $ show det.perExtraKmRate

          includedKmPerHrCaption = show Tags.PER_HOUR_DISTANCE_KM
          includedKmPerHrItem = mkBreakupItem includedKmPerHrCaption . mkValue $ show det.includedKmPerHr

          plannedPerKmRateCaption = show Tags.PLANNED_PER_KM_CHARGE
          plannedPerKmRateItem = mkBreakupItem plannedPerKmRateCaption . mkValue $ show det.plannedPerKmRate

          pickupChargeCaption = show Tags.DEAD_KILOMETER_FARE
          pickupChargeBreakup = mkBreakupItem pickupChargeCaption (mkValue $ show det.deadKmFare)

      [minFareItem, perHourChargeItem, perExtraMinRateItem, perExtraKmRateItem, includedKmPerHrItem, plannedPerKmRateItem, pickupChargeBreakup]
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalInterCityBreakups det = do
      let minFareCaption = show Tags.MIN_FARE
          minFareItem = mkBreakupItem minFareCaption . mkValue $ show det.baseFare

          perHourChargeCaption = show Tags.PER_HOUR_CHARGE
          perHourChargeItem = mkBreakupItem perHourChargeCaption . mkValue $ show det.perHourCharge

          perExtraMinRateCaption = show Tags.PER_MINUTE_CHARGE
          perExtraMinRateItem = mkBreakupItem perExtraMinRateCaption . mkValue $ show det.perExtraMinRate

          perExtraKmRateCaption = show Tags.UNPLANNED_PER_KM_CHARGE
          perExtraKmRateItem = mkBreakupItem perExtraKmRateCaption . mkValue $ show det.perExtraKmRate

          includedKmPerHrCaption = show Tags.PER_HOUR_DISTANCE_KM
          includedKmPerHrItem = mkBreakupItem includedKmPerHrCaption . mkValue $ show det.kmPerPlannedExtraHour

          plannedPerKmRateCaption = show Tags.PLANNED_PER_KM_CHARGE
          plannedPerKmRateItem = mkBreakupItem plannedPerKmRateCaption . mkValue $ show det.perKmRateOneWay

          plannedPerKmRateRoundCaption = show Tags.PLANNED_PER_KM_CHARGE_ROUND_TRIP
          plannedPerKmRateRoundItem = mkBreakupItem plannedPerKmRateRoundCaption . mkValue $ show det.perKmRateRoundTrip

          perDayMaxHourAllowanceCaption = show Tags.PER_DAY_MAX_HOUR_ALLOWANCE
          perDayMaxHourAllowanceItem = mkBreakupItem perDayMaxHourAllowanceCaption . mkValue $ show det.perDayMaxHourAllowance

          perDayMaxAllowanceInMinutesCaption = show Tags.PER_DAY_MAX_ALLOWANCE_IN_MINS
          perDayMaxAllowanceInMinutesItem = mkBreakupItem perDayMaxAllowanceInMinutesCaption . mkValue <$> minutesToText det.perDayMaxAllowanceInMins

          pickupChargeCaption = show Tags.DEAD_KILOMETER_FARE
          pickupChargeBreakup = mkBreakupItem pickupChargeCaption (mkValue $ show det.deadKmFare)

      [minFareItem, pickupChargeBreakup, perHourChargeItem, perExtraMinRateItem, perExtraKmRateItem, includedKmPerHrItem, plannedPerKmRateItem, plannedPerKmRateRoundItem, perDayMaxHourAllowanceItem]
        <> catMaybes [perDayMaxAllowanceInMinutesItem]
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalProgressiveBreakups det = do
      let perExtraKmFareSections = NE.nubBy (\x y -> x.perExtraKmRate == y.perExtraKmRate) $ NE.sortBy (comparing (.startDistance)) det.perExtraKmRateSections

          mkPerExtraKmFareItem section = do
            let perExtraKmFareCaption = show Tags.EXTRA_PER_KM_FARE
            mkBreakupItem perExtraKmFareCaption (mkValue $ highPrecMoneyToText section.perExtraKmRate)
          perExtraKmFareItems = mkPerExtraKmFareItem <$> toList perExtraKmFareSections

          perExtraKmStepFareItems = mkPerExtraKmStepFareItem [] (toList perExtraKmFareSections) det.baseDistance.getMeters

          perMinRateSections = List.sortBy (comparing (.rideDurationInMin)) $ maybe [] toList det.perMinRateSections
          perMinStepFareItems = mkPerMinStepFareItem [] perMinRateSections

          baseDistanceCaption = show Tags.BASE_DISTANCE
          baseDistanceBreakup = mkBreakupItem baseDistanceCaption (mkValue $ show det.baseDistance)

          baseFareCaption = show Tags.BASE_FARE
          baseFareBreakup = mkBreakupItem baseFareCaption (mkValue $ show det.baseFare)

          pickupChargeCaption = show Tags.DEAD_KILOMETER_FARE
          pickupChargeBreakup = mkBreakupItem pickupChargeCaption (mkValue $ show det.deadKmFare)

      [baseDistanceBreakup, baseFareBreakup, pickupChargeBreakup]
        <> perExtraKmFareItems
        <> perExtraKmStepFareItems
        <> perMinStepFareItems
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)
      where
        mkPerExtraKmStepFareItem perExtraKmStepFareItems [] _ = perExtraKmStepFareItems
        mkPerExtraKmStepFareItem perExtraKmStepFareItems [s1] baseDistance = do
          let startDistance = s1.startDistance.getMeters + baseDistance
              perExtraKmStepFareCaption = show $ Tags.EXTRA_PER_KM_STEP_FARE startDistance Nothing
              perExtraKmStepFareItem = mkBreakupItem perExtraKmStepFareCaption (mkValue $ highPrecMoneyToText s1.perExtraKmRate)
          perExtraKmStepFareItems <> [perExtraKmStepFareItem]
        mkPerExtraKmStepFareItem perExtraKmStepFareItems (s1 : s2 : ss) baseDistance = do
          let startDistance = s1.startDistance.getMeters + baseDistance
              endDistance = s2.startDistance.getMeters + baseDistance
              perExtraKmStepFareCaption = show $ Tags.EXTRA_PER_KM_STEP_FARE startDistance (Just endDistance)
              perExtraKmStepFareItem = mkBreakupItem perExtraKmStepFareCaption (mkValue $ highPrecMoneyToText s1.perExtraKmRate)
          mkPerExtraKmStepFareItem (perExtraKmStepFareItems <> [perExtraKmStepFareItem]) (s2 : ss) baseDistance

        mkPerMinStepFareItem perMinStepFareItems [] = perMinStepFareItems
        mkPerMinStepFareItem perMinStepFareItems [fp1] = do
          let sectionStartDuration = fp1.rideDurationInMin
              perMinStepFareCaption = show $ Tags.PER_MINUTE_STEP_FARE sectionStartDuration Nothing
              perMinStepFareItem = mkBreakupItem perMinStepFareCaption (mkValue $ highPrecMoneyToText fp1.perMinRate.amount)
          perMinStepFareItems <> [perMinStepFareItem]
        mkPerMinStepFareItem perMinStepFareItems (fp1 : fp2 : fps) = do
          let sectionStartDuration = fp1.rideDurationInMin
              sectionEndDuration = fp2.rideDurationInMin
              perMinStepFareCaption = show $ Tags.PER_MINUTE_STEP_FARE sectionStartDuration (Just sectionEndDuration)
              perMinStepFareItem = mkBreakupItem perMinStepFareCaption (mkValue $ highPrecMoneyToText fp1.perMinRate.amount)
          mkPerMinStepFareItem (perMinStepFareItems <> [perMinStepFareItem]) (fp2 : fps)

    mkAdditionalSlabBreakups det = do
      let baseDistanceCaption = show Tags.BASE_DISTANCE
          baseDistanceBreakup = mkBreakupItem baseDistanceCaption (mkValue $ show det.startDistance)

          baseFareCaption = show Tags.BASE_FARE
          baseFareBreakup = mkBreakupItem baseFareCaption (mkValue $ show det.baseFare)

      [baseDistanceBreakup, baseFareBreakup]
        <> (platformFeeBreakups det.platformFeeInfo)
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalAmbulanceBreakups det = do
      let vehicleAgeSections = NE.sortBy (comparing (.vehicleAge.getMonths)) det.slabs
          perKmStepFareItems = mkPerKmStepFareItem [] (toList vehicleAgeSections)
          baseStepFareItems = mkBaseStepFareItem [] (toList vehicleAgeSections)
          baseStepDistanceItems = mkBaseStepDistanceItem [] (toList vehicleAgeSections)
          nightShiftFareItems = mkNightShiftStepFareItem [] (toList vehicleAgeSections)
          platformFeeItems = mkPlatformFeeStepFareItem [] (toList vehicleAgeSections)
          waitingChargeItems = mkWaitingChargeStepFareItem [] (toList vehicleAgeSections)

      perKmStepFareItems
        <> baseStepFareItems
        <> baseStepDistanceItems
        <> nightShiftFareItems
        <> platformFeeItems
        <> waitingChargeItems
      where
        mkPerKmStepFareItem perKmStepFareItems [] = perKmStepFareItems
        mkPerKmStepFareItem perKmStepFareItems [s1] = do
          let perKmStepFareItem = mkBreakupItem (show $ Tags.PER_KM_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ highPrecMoneyToText s1.perKmRate)
          perKmStepFareItems <> [perKmStepFareItem]
        mkPerKmStepFareItem perKmStepFareItems (s1 : s2 : ss) = do
          let perKmStepFareItem = mkBreakupItem (show $ Tags.PER_KM_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) (mkValue $ highPrecMoneyToText s1.perKmRate)
          mkPerKmStepFareItem (perKmStepFareItems <> [perKmStepFareItem]) (s2 : ss)

        mkBaseStepFareItem baseStepFareItems [] = baseStepFareItems
        mkBaseStepFareItem baseStepFareItems [s1] = do
          let baseStepFareItem = mkBreakupItem (show $ Tags.BASE_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ highPrecMoneyToText s1.baseFare)
          baseStepFareItems <> [baseStepFareItem]
        mkBaseStepFareItem baseStepFareItems (s1 : s2 : ss) = do
          let baseStepFareCaption = show $ Tags.BASE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)
              baseStepFareItem = mkBreakupItem baseStepFareCaption (mkValue $ highPrecMoneyToText s1.baseFare)
          mkBaseStepFareItem (baseStepFareItems <> [baseStepFareItem]) (s2 : ss)

        mkBaseStepDistanceItem baseStepDistanceItems [] = baseStepDistanceItems
        mkBaseStepDistanceItem baseStepDistanceItems [s1] = do
          let baseStepDistanceItem = mkBreakupItem (show $ Tags.BASE_STEP_DISTANCE s1.vehicleAge.getMonths Nothing) (mkValue $ show s1.baseDistance.getMeters)
          baseStepDistanceItems <> [baseStepDistanceItem]
        mkBaseStepDistanceItem baseStepDistanceItems (s1 : s2 : ss) = do
          let baseStepFareCaption = show $ Tags.BASE_STEP_DISTANCE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)
              baseStepDistanceItem = mkBreakupItem baseStepFareCaption (mkValue $ show s1.baseDistance.getMeters)
          mkBaseStepDistanceItem (baseStepDistanceItems <> [baseStepDistanceItem]) (s2 : ss)

        mkNightShiftStepFareItem nightShiftStepFareItems [] = nightShiftStepFareItems
        mkNightShiftStepFareItem nightShiftStepFareItems [s1] = do
          case s1.nightShiftCharge of
            Nothing -> nightShiftStepFareItems
            Just nightShiftCharge -> do
              let (mbNightShiftChargePercentage, mbConstantNightShiftCharge) = getNewNightShiftCharge nightShiftCharge
                  nightShiftPercentageStepFareItem = mkBreakupItem (show $ Tags.NIGHT_SHIFT_STEP_PERCENTAGE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbNightShiftChargePercentage
                  nightShiftConstantStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_NIGHT_SHIFT_STEP_CHARGE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbConstantNightShiftCharge

              nightShiftStepFareItems <> catMaybes [nightShiftPercentageStepFareItem, nightShiftConstantStepFareItem]
        mkNightShiftStepFareItem nightShiftStepFareItems (s1 : s2 : ss) = do
          case s1.nightShiftCharge of
            Nothing -> nightShiftStepFareItems
            Just nightShiftCharge -> do
              let (mbNightShiftChargePercentage, mbConstantNightShiftCharge) = getNewNightShiftCharge nightShiftCharge
                  nightShiftPercentageStepFareItem = mkBreakupItem (show $ Tags.NIGHT_SHIFT_STEP_PERCENTAGE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbNightShiftChargePercentage
                  nightShiftConstantStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_NIGHT_SHIFT_STEP_CHARGE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbConstantNightShiftCharge
              mkNightShiftStepFareItem (nightShiftStepFareItems <> catMaybes [nightShiftPercentageStepFareItem, nightShiftConstantStepFareItem]) (s2 : ss)

        mkPlatformFeeStepFareItem platformFeeStepFareItems [] = platformFeeStepFareItems
        mkPlatformFeeStepFareItem platformFeeStepFareItems [s1] = do
          case s1.platformFeeInfo of
            Nothing -> platformFeeStepFareItems
            Just platformFeeInfo -> do
              let (mbProgressivePlatformFee, mbConstantPlatformFee) = getPlatformFee platformFeeInfo

                  mbProgressivePlatformFeeStepFareItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbProgressivePlatformFee
                  mbConstantPlatformFeeItem = mkBreakupItem (show $ Tags.CONSTANT_PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbConstantPlatformFee
                  mbPlatformFeeCgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_CGST_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ show platformFeeInfo.cgst)
                  mbPlatformFeeSgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_SGST_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ show platformFeeInfo.sgst)

              platformFeeStepFareItems <> catMaybes [mbProgressivePlatformFeeStepFareItem, mbConstantPlatformFeeItem, Just mbPlatformFeeCgstItem, Just mbPlatformFeeSgstItem]
        mkPlatformFeeStepFareItem platformFeeStepFareItems (s1 : s2 : ss) = do
          case s1.platformFeeInfo of
            Nothing -> platformFeeStepFareItems
            Just platformFeeInfo -> do
              let (mbProgressivePlatformFee, mbConstantPlatformFee) = getPlatformFee platformFeeInfo

                  mbProgressivePlatformFeeStepFareItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbProgressivePlatformFee
                  mbConstantPlatformFeeItem = mkBreakupItem (show $ Tags.CONSTANT_PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbConstantPlatformFee
                  mbPlatformFeeCgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_CGST_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) (mkValue $ show platformFeeInfo.cgst)
                  mbPlatformFeeSgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_SGST_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) (mkValue $ show platformFeeInfo.sgst)

              mkPlatformFeeStepFareItem (platformFeeStepFareItems <> catMaybes [mbProgressivePlatformFeeStepFareItem, mbConstantPlatformFeeItem, Just mbPlatformFeeCgstItem, Just mbPlatformFeeSgstItem]) (s2 : ss)

        mkWaitingChargeStepFareItem waitingChargeStepFareItems [] = waitingChargeStepFareItems
        mkWaitingChargeStepFareItem waitingChargeStepFareItems [s1] = do
          case s1.waitingChargeInfo of
            Nothing -> waitingChargeStepFareItems
            Just waitingChargeInfo -> do
              let (_, mbWaitingChargePerMinFloat, mbConstantWaitingCharges) = getWaitingCharge waitingChargeInfo

                  mbWaitingChargePerMinFloatStepFareItem = mkBreakupItem (show $ Tags.WAITING_CHARGE_PER_MIN_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbWaitingChargePerMinFloat
                  mbConstantWaitingChargeStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_WAITING_CHARGE_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbConstantWaitingCharges
                  freeWaitingTimeStepFareItem = mkBreakupItem (show $ Tags.FREE_WAITING_TIME_STEP_MINUTES s1.vehicleAge.getMonths Nothing) . mkValue . show $ waitingChargeInfo.freeWaitingTime.getMinutes

              waitingChargeStepFareItems <> catMaybes [mbWaitingChargePerMinFloatStepFareItem, mbConstantWaitingChargeStepFareItem, Just freeWaitingTimeStepFareItem]
        mkWaitingChargeStepFareItem waitingChargeStepFareItems (s1 : s2 : ss) = do
          case s1.waitingChargeInfo of
            Nothing -> waitingChargeStepFareItems
            Just waitingChargeInfo -> do
              let (_, mbWaitingChargePerMinFloat, mbConstantWaitingCharges) = getWaitingCharge waitingChargeInfo

                  mbWaitingChargePerMinFloatStepFareItem = mkBreakupItem (show $ Tags.WAITING_CHARGE_PER_MIN_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbWaitingChargePerMinFloat
                  mbConstantWaitingChargeStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_WAITING_CHARGE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbConstantWaitingCharges
                  freeWaitingTimeStepFareItem = mkBreakupItem (show $ Tags.FREE_WAITING_TIME_STEP_MINUTES s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue . show $ waitingChargeInfo.freeWaitingTime.getMinutes

              mkWaitingChargeStepFareItem (waitingChargeStepFareItems <> catMaybes [mbWaitingChargePerMinFloatStepFareItem, mbConstantWaitingChargeStepFareItem, Just freeWaitingTimeStepFareItem]) (s2 : ss)

    platformFeeBreakups Nothing = []
    platformFeeBreakups (Just platformFeeInfo) = do
      let (mbProgressivePlatformFee, mbConstantPlatformFee) = getPlatformFee platformFeeInfo

          mbProgressivePlatformFeeCaption = show Tags.PROGRESSIVE_PLATFORM_CHARGE
          mbProgressivePlatformFeeItem = mkBreakupItem mbProgressivePlatformFeeCaption . mkValue <$> mbProgressivePlatformFee

          mbConstantPlatformFeeCation = show Tags.CONSTANT_PLATFORM_CHARGE
          mbConstantPlatformFeeItem = mkBreakupItem mbConstantPlatformFeeCation . mkValue <$> mbConstantPlatformFee

          mbPlatformFeeCgstCation = show Tags.PLATFORM_FEE_CGST
          mbPlatformFeeCgstItem = mkBreakupItem mbPlatformFeeCgstCation (mkValue $ show platformFeeInfo.cgst)

          mbPlatformFeeSgstCation = show Tags.PLATFORM_FEE_SGST
          mbPlatformFeeSgstItem = mkBreakupItem mbPlatformFeeSgstCation (mkValue $ show platformFeeInfo.sgst)

      catMaybes [mbProgressivePlatformFeeItem, mbConstantPlatformFeeItem, Just mbPlatformFeeCgstItem, Just mbPlatformFeeSgstItem]

    waitingChargeBreakups Nothing = []
    waitingChargeBreakups (Just waitingChargeInfo) = do
      let (mbWaitingChargePerMin, mbWaitingChargePerMinFloat, mbConstantWaitingCharges) = getWaitingCharge waitingChargeInfo

          waitingOrPickupChargesCaption = show Tags.WAITING_OR_PICKUP_CHARGES -- TODO :: To be deprecated
          mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkValue <$> mbConstantWaitingCharges

          mbWaitingChargePerMinCaption = show Tags.WAITING_CHARGE_PER_MIN
          mbWaitingChargePerMinItem = mkBreakupItem mbWaitingChargePerMinCaption . mkValue <$> mbWaitingChargePerMin

          mbWaitingChargePerMinFloatCaption = show Tags.WAITING_CHARGE_RATE_PER_MIN
          mbWaitingChargePerMinFloatItem = mkBreakupItem mbWaitingChargePerMinFloatCaption . mkValue <$> mbWaitingChargePerMinFloat

          mbConstantWaitingChargeCaption = show Tags.CONSTANT_WAITING_CHARGE
          mbConstantWaitingChargeItem = mkBreakupItem mbConstantWaitingChargeCaption . mkValue <$> mbConstantWaitingCharges

          freeWaitingTimeCation = show Tags.FREE_WAITING_TIME_IN_MINUTES
          freeWaitingTimeItem = mkBreakupItem freeWaitingTimeCation . mkValue . show $ waitingChargeInfo.freeWaitingTime.getMinutes

      catMaybes [mbWaitingOrPickupChargesItem, mbWaitingChargePerMinItem, mbWaitingChargePerMinFloatItem, mbConstantWaitingChargeItem, Just freeWaitingTimeItem]

    oldNightShiftChargeBreakups nightShiftChargeInfo = do
      let getNightShiftChargeValue (FarePolicyD.ProgressiveNightShiftCharge a) = highPrecMoneyToText $ toHighPrecMoney a -- fix from customer side first
          getNightShiftChargeValue (FarePolicyD.ConstantNightShiftCharge a) = show a

      let oldNightShiftCharge = getNightShiftChargeValue <$> nightShiftChargeInfo
          oldNightShiftChargeCaption = show Tags.NIGHT_SHIFT_CHARGE
          oldNightShiftChargeItem = mkBreakupItem oldNightShiftChargeCaption . mkValue <$> oldNightShiftCharge

      catMaybes [oldNightShiftChargeItem]

    newNightShiftChargeBreakups Nothing = []
    newNightShiftChargeBreakups (Just nightShiftChargeInfo) = do
      let (mbNightShiftChargePercentage, mbConstantNightShiftCharge) = getNewNightShiftCharge nightShiftChargeInfo

          mbNightShiftChargePercentageCaption = show Tags.NIGHT_SHIFT_CHARGE_PERCENTAGE
          mbNightShiftChargePercentageItem = mkBreakupItem mbNightShiftChargePercentageCaption . mkValue <$> mbNightShiftChargePercentage

          mbConstantNightShiftChargeCaption = show Tags.CONSTANT_NIGHT_SHIFT_CHARGE
          mbConstantNightShiftChargeItem = mkBreakupItem mbConstantNightShiftChargeCaption . mkValue <$> mbConstantNightShiftCharge

      catMaybes [mbNightShiftChargePercentageItem, mbConstantNightShiftChargeItem]

    getNewNightShiftCharge nightShiftCharge =
      nightShiftCharge & \case
        FarePolicyD.ProgressiveNightShiftCharge a -> (Just $ show ((a - 1) * 100), Nothing)
        FarePolicyD.ConstantNightShiftCharge a -> (Nothing, Just $ show a)

    getWaitingCharge waitingChargeInfo =
      waitingChargeInfo.waitingCharge & \case
        FarePolicyD.PerMinuteWaitingCharge hpm -> (Just $ highPrecMoneyToText hpm, Just $ show hpm, Nothing)
        FarePolicyD.ConstantWaitingCharge mo -> (Nothing, Nothing, Just $ show mo)

    getPlatformFee platformFeeInfo =
      platformFeeInfo.platformFeeCharge & \case
        FarePolicyD.ProgressivePlatformFee ppf -> (Just $ show ppf, Nothing)
        FarePolicyD.ConstantPlatformFee cpf -> (Nothing, Just $ show cpf)

    mkDriverExtraFeeBoundsMinFeeItem driverExtraFeeBoundsMinFeeItems [] _ = driverExtraFeeBoundsMinFeeItems
    mkDriverExtraFeeBoundsMinFeeItem driverExtraFeeBoundsMinFeeItems [s1] startDistance = do
      let driverExtraFeeBoundMinFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE startDistance Nothing
          driverExtraFeeBoundMinFeeItem = mkBreakupItem driverExtraFeeBoundMinFeeCaption (mkValue $ highPrecMoneyToText s1.minFee)
      driverExtraFeeBoundsMinFeeItems <> [driverExtraFeeBoundMinFeeItem]
    mkDriverExtraFeeBoundsMinFeeItem driverExtraFeeBoundsMinFeeItems (s1 : s2 : ss) startDistance = do
      let driverExtraFeeBoundMinFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE startDistance (Just s2.startDistance.getMeters)
          driverExtraFeeBoundMinFeeItem = mkBreakupItem driverExtraFeeBoundMinFeeCaption (mkValue $ highPrecMoneyToText s1.minFee)
      mkDriverExtraFeeBoundsMinFeeItem (driverExtraFeeBoundsMinFeeItems <> [driverExtraFeeBoundMinFeeItem]) (s2 : ss) s2.startDistance.getMeters

    mkDriverExtraFeeBoundsMaxFeeItem driverExtraFeeBoundsMaxFeeItems [] _ = driverExtraFeeBoundsMaxFeeItems
    mkDriverExtraFeeBoundsMaxFeeItem driverExtraFeeBoundsMaxFeeItems [s1] startDistance = do
      let driverExtraFeeBoundMaxFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE startDistance Nothing
          driverExtraFeeBoundMaxFeeItem = mkBreakupItem driverExtraFeeBoundMaxFeeCaption (mkValue $ highPrecMoneyToText s1.maxFee)
      driverExtraFeeBoundsMaxFeeItems <> [driverExtraFeeBoundMaxFeeItem]
    mkDriverExtraFeeBoundsMaxFeeItem driverExtraFeeBoundsMaxFeeItems (s1 : s2 : ss) startDistance = do
      let driverExtraFeeBoundMaxFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE startDistance (Just s2.startDistance.getMeters)
          driverExtraFeeBoundMaxFeeItem = mkBreakupItem driverExtraFeeBoundMaxFeeCaption (mkValue $ highPrecMoneyToText s1.maxFee)
      mkDriverExtraFeeBoundsMaxFeeItem (driverExtraFeeBoundsMaxFeeItems <> [driverExtraFeeBoundMaxFeeItem]) (s2 : ss) s2.startDistance.getMeters

utcToIst :: Minutes -> UTCTime -> LocalTime
utcToIst timeZoneDiff utcTime = utcToLocalTime (minutesToTimeZone timeZoneDiff.getMinutes) utcTime -- IST is UTC + 5:30

minutesToText :: Maybe Minutes -> Maybe Text
minutesToText mbMinutes = case mbMinutes of
  Just minutes -> Just $ show minutes.getMinutes
  _ -> Nothing

localTimeToDayOfWeekAndHour :: LocalTime -> (DayOfWeek, Int)
localTimeToDayOfWeekAndHour localTime =
  let (year, month, day) = toGregorian $ localDay localTime
      (_, _, dayOfWeekNum) = toWeekDate $ fromGregorian year month day
      dayWeek = toEnum dayOfWeekNum :: DayOfWeek
      hourOfDay = todHour $ localTimeOfDay localTime
   in (dayWeek, hourOfDay)

-- | First-Just short-circuit over a list of monadic lookups: runs each action
-- in order and stops as soon as one yields 'Just', skipping the remaining IO.
firstJustIO :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustIO [] = pure Nothing
firstJustIO (x : xs) = x >>= maybe (firstJustIO xs) (pure . Just)

-- | Congestion multipliers are pre-aggregated averages written by the
-- CongestionChargeAvg allocator job. Read them most-specific-first and stop at
-- the first level that has a value: geohash+distanceBin -> geohash -> city.
getCongestionMultiplier ::
  (MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Text ->
  m (Maybe Double, Maybe Double)
getCongestionMultiplier geohash distanceBin cityId = do
  (congestionMultiplier :: Maybe Double) <-
    firstJustIO
      [ Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohashAndDistanceBin geohash distanceBin,
        Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohash geohash,
        Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithCity cityId
      ]
  (congestionMultiplierPast :: Maybe Double) <-
    firstJustIO
      [ Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohashAndDistanceBinPast geohash distanceBin,
        Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohashPast geohash,
        Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithCityPast cityId
      ]
  return (congestionMultiplier, congestionMultiplierPast)

-- | All Redis-derived inputs (plus the random toss) that the dynamic-pricing
-- logic consumes. congestion / rain / toss are independent of vehicleCategory;
-- QAR and supply-demand are keyed by it. Splitting the fetch this way lets the
-- search path resolve the shared part once and the per-category part once per
-- distinct vehicleCategory, instead of re-hitting Redis for every service tier.
data DynamicPricingInputs = DynamicPricingInputs
  { actualQAR :: Maybe Double,
    actualQARPast :: Maybe Double,
    mbSupplyDemandRatioFromLoc :: Maybe Double,
    mbSupplyDemandRatioToLoc :: Maybe Double,
    congestionMultiplier :: Maybe Double,
    congestionMultiplierPast :: Maybe Double,
    mbRainStatus :: Maybe Text,
    toss :: Int
  }

-- vehicleCategory-independent inputs: congestion (geohash+distanceBin), rain
-- (geohash) and the per-request random toss. Fetched once per search.
fetchSharedDynamicPricingInputs ::
  (MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Text ->
  m (Maybe Double, Maybe Double, Maybe Text, Int)
fetchSharedDynamicPricingInputs geohash distanceBin cityId = do
  (congestionMultiplier, congestionMultiplierPast) <- getCongestionMultiplier geohash distanceBin cityId
  mbRainStatus <- Hedis.withCrossAppRedis $ Hedis.get $ mkRainStatusKey geohash
  toss <- getRandomInRange (1, 100 :: Int)
  pure (congestionMultiplier, congestionMultiplierPast, mbRainStatus, toss)

-- vehicleCategory-dependent inputs: QAR and the supply-demand ratios.
fetchCategoryDynamicPricingInputs ::
  (MonadFlow m, CacheFlow m r) =>
  UTCTime ->
  LatLong ->
  Double ->
  Text ->
  Maybe Text ->
  Int ->
  Text ->
  Maybe DVC.VehicleCategory ->
  m (Maybe Double, Maybe Double, Maybe Double, Maybe Double)
fetchCategoryDynamicPricingInputs now location radius geohash mbToLocGeohash distance cityId vehicleCategory = do
  (actualQAR, actualQARPast) <- getActualQAR now vehicleCategory location radius distance cityId
  mbSupplyDemandRatioFromLoc <- Hedis.withCrossAppRedis $ Hedis.get $ mkSupplyDemandRatioKeyWithGeohash geohash vehicleCategory
  mbSupplyDemandRatioToLoc <- join <$> traverse (\g -> Hedis.withCrossAppRedis $ Hedis.get $ mkSupplyDemandRatioKeyWithGeohash g vehicleCategory) mbToLocGeohash
  pure (actualQAR, actualQARPast, mbSupplyDemandRatioFromLoc, mbSupplyDemandRatioToLoc)

mkDynamicPricingInputs ::
  (Maybe Double, Maybe Double, Maybe Text, Int) ->
  (Maybe Double, Maybe Double, Maybe Double, Maybe Double) ->
  DynamicPricingInputs
mkDynamicPricingInputs (congestionMultiplier, congestionMultiplierPast, mbRainStatus, toss) (actualQAR, actualQARPast, mbSupplyDemandRatioFromLoc, mbSupplyDemandRatioToLoc) =
  DynamicPricingInputs {..}

-- Single-vehicleCategory resolution, used on the single-tier (endRide / quote)
-- paths where there is nothing to dedup against.
resolveDynamicPricingInputs ::
  (MonadFlow m, CacheFlow m r) =>
  UTCTime ->
  LatLong ->
  Double ->
  Text ->
  Maybe Text ->
  Int ->
  Text ->
  Maybe DVC.VehicleCategory ->
  m DynamicPricingInputs
resolveDynamicPricingInputs now location radius geohash mbToLocGeohash distance cityId vehicleCategory = do
  shared <- fetchSharedDynamicPricingInputs geohash (getDistanceBin distance) cityId
  categoryInputs <- fetchCategoryDynamicPricingInputs now location radius geohash mbToLocGeohash distance cityId vehicleCategory
  pure $ mkDynamicPricingInputs shared categoryInputs

-- Deduped resolution for a whole search: the shared part is fetched once and
-- the per-category part once per distinct vehicleCategory. The result is a
-- lookup list passed into 'getCongestionChargeMultiplierFromModel'' per tier.
buildDynamicPricingInputs ::
  (MonadFlow m, CacheFlow m r) =>
  UTCTime ->
  LatLong ->
  Double ->
  Text ->
  Maybe Text ->
  Int ->
  Text ->
  [Maybe DVC.VehicleCategory] ->
  m [(Maybe DVC.VehicleCategory, DynamicPricingInputs)]
buildDynamicPricingInputs now location radius geohash mbToLocGeohash distance cityId vehicleCategories = do
  shared <- fetchSharedDynamicPricingInputs geohash (getDistanceBin distance) cityId
  forM (List.nub vehicleCategories) $ \vehicleCategory -> do
    categoryInputs <- fetchCategoryDynamicPricingInputs now location radius geohash mbToLocGeohash distance cityId vehicleCategory
    pure (vehicleCategory, mkDynamicPricingInputs shared categoryInputs)

getCongestionChargeMultiplierFromModel' ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  Maybe DynamicPricingInputs ->
  Seconds ->
  Maybe LatLong ->
  Maybe Text ->
  Maybe Text ->
  ServiceTierType ->
  Maybe DVC.VehicleCategory ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Bool ->
  Maybe Double ->
  Maybe Text ->
  Maybe Int ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Seconds ->
  Maybe Seconds ->
  m (Maybe CongestionChargeDetailsModel)
getCongestionChargeMultiplierFromModel' mbDpInputs timeDiffFromUtc (Just _fromLocation) (Just fromLocGeohash) toLocGeohash serviceTier vehicleCategory (Just (Meters distance)) (Just (Seconds duration)) (Just True) _radius' mbSpecialLocName (Just dynamicPricingLogicVersion) merchantOperatingCityId mbEstimatedDuration mbActualDuration = do
  localTime <- getLocalCurrentTime timeDiffFromUtc
  logInfo $ "Calling DynamicPricing" <> show fromLocGeohash
  now <- getCurrentTime
  let distanceInKm = int2Double distance / 1000.0
      estimatedDurationInH = int2Double duration / 3600.0
      speedKmh = if estimatedDurationInH == 0.0 then 0.0 else distanceInKm / estimatedDurationInH
  -- All Redis-derived inputs (QAR, supply-demand, congestion, rain) plus the
  -- random toss. On the search path these are precomputed once per
  -- vehicleCategory in 'getAllFarePoliciesProduct' and passed in via 'mbDpInputs';
  -- on the single-tier paths we resolve them here for this vehicleCategory.
  DynamicPricingInputs {..} <- maybe (resolveDynamicPricingInputs now _fromLocation (fromMaybe 5.0 _radius') fromLocGeohash toLocGeohash distance merchantOperatingCityId.getId vehicleCategory) pure mbDpInputs

  -- 'getActualQAR'/'getCongestionMultiplier' already resolve the
  -- geohash+distanceBin -> geohash -> city hierarchy internally and return the
  -- single winning current/past value. The granular per-level fields of
  -- 'CongestionChargeData' (and the model) are no longer computed separately, so
  -- we record the resolved values under the geohash-level fields (the natural
  -- representative) and leave the rest 'Nothing'. These feed the
  -- record-wildcards below.
  let mbActualQARFromLocGeohash = actualQAR
      mbActualQARFromLocGeohashPast = actualQARPast
      mbActualQARFromLocGeohashDistance = Nothing
      mbActualQARFromLocGeohashDistancePast = Nothing
      mbActualQARCity = Nothing
      mbActualQARCityPast = Nothing
      mbCongestionFromLocGeohash = congestionMultiplier
      mbCongestionFromLocGeohashPast = congestionMultiplierPast
      mbCongestionFromLocGeohashDistance = Nothing
      mbCongestionFromLocGeohashDistancePast = Nothing
      mbCongestionCity = Nothing
      mbCongestionCityPast = Nothing

  (allLogics, mbVersion) <- getAppDynamicLogic (cast merchantOperatingCityId) LYT.DYNAMIC_PRICING_UNIFIED localTime (Just dynamicPricingLogicVersion) Nothing
  let estimatedDurationInS = fmap (\(Seconds s) -> s) mbEstimatedDuration
      actualDurationInS = fmap (\(Seconds s) -> s) mbActualDuration
      dynamicPricingData = DynamicPricingData {serviceTier, speedKmh, distanceInKm, supplyDemandRatioFromLoc = fromMaybe 0.0 mbSupplyDemandRatioFromLoc, supplyDemandRatioToLoc = fromMaybe 0.0 mbSupplyDemandRatioToLoc, toss, actualQAR = actualQAR, actualQARPast = actualQARPast, congestionMultiplier = congestionMultiplier, congestionMultiplierPast = congestionMultiplierPast, rainStatus = mbRainStatus, mbSpecialLocName = mbSpecialLocName, estimatedDurationInS = estimatedDurationInS, actualDurationInS = actualDurationInS}
  if null allLogics
    then do
      logInfo $ "No DynamicPricingLogics found for merchantOperatingCityId : " <> show merchantOperatingCityId <> " and serviceTier : " <> show serviceTier <> " and localTime : " <> show localTime
      return Nothing
    else do
      response <- withTryCatch "runLogics:getCongestionChargeMultiplierFromModel" $ LYDL.runLogicsWithDebugLog LYDL.Driver (cast merchantOperatingCityId) LYT.DYNAMIC_PRICING_UNIFIED Nothing allLogics dynamicPricingData
      logInfo $ "DynamicPricing Req Logics : " <> show allLogics <> " and data is : " <> show dynamicPricingData <> " and response is : " <> show response
      case response of
        Left e -> do
          logError $ "Error in running DynamicPricingLogics - " <> show e <> " - " <> show dynamicPricingData <> " - " <> show allLogics
          return Nothing
        Right resp ->
          case (A.fromJSON resp.result :: Result DynamicPricingResult) of
            A.Success result -> do
              case (result.congestionFeePerMin, result.smartTipSuggestion, result.congestionChargeMultiplier) of
                (Just congestionFee, smartTip, Nothing) -> do
                  let mbVersionText = fmap (T.pack . show) mbVersion <|> result.version
                  return $
                    mbVersionText <&> \version -> do
                      CongestionChargeDetailsModel
                        { dpVersion = Just version,
                          congestionChargePerMin = Just congestionFee,
                          smartTipSuggestion = smartTip,
                          smartTipReason = result.smartTipReason,
                          congestionChargeMultiplier = Nothing,
                          driverExtraFeeBounds = result.driverExtraFeeBounds,
                          congestionChargeData = FarePolicyD.CongestionChargeData {..},
                          ..
                        }
                (Nothing, smartTip, Just congestionChargeMultiplier) -> do
                  let mbVersionText = fmap (T.pack . show) mbVersion <|> result.version
                  return $
                    mbVersionText <&> \version -> do
                      CongestionChargeDetailsModel
                        { dpVersion = Just version,
                          congestionChargePerMin = Nothing,
                          smartTipSuggestion = smartTip,
                          smartTipReason = result.smartTipReason,
                          congestionChargeMultiplier = Just congestionChargeMultiplier,
                          congestionChargeData = FarePolicyD.CongestionChargeData {..},
                          driverExtraFeeBounds = result.driverExtraFeeBounds,
                          ..
                        }
                (_, Just smartTipSuggestion, _) -> do
                  let mbVersionText = fmap (T.pack . show) mbVersion <|> result.version
                  return $
                    mbVersionText <&> \version -> do
                      CongestionChargeDetailsModel
                        { dpVersion = Just version,
                          congestionChargePerMin = Nothing,
                          smartTipSuggestion = Just smartTipSuggestion,
                          smartTipReason = result.smartTipReason,
                          congestionChargeMultiplier = Nothing,
                          congestionChargeData = FarePolicyD.CongestionChargeData {..},
                          driverExtraFeeBounds = result.driverExtraFeeBounds,
                          ..
                        }
                _ -> do
                  return $
                    Just $
                      CongestionChargeDetailsModel
                        { dpVersion = Nothing,
                          congestionChargePerMin = Nothing,
                          smartTipSuggestion = Nothing,
                          smartTipReason = Nothing,
                          congestionChargeMultiplier = Nothing,
                          congestionChargeData = FarePolicyD.CongestionChargeData {..},
                          driverExtraFeeBounds = result.driverExtraFeeBounds,
                          ..
                        }
            A.Error err -> do
              logWarning $ "Error in parsing DynamicPricingResult - " <> show err <> " - " <> show resp <> " - " <> show dynamicPricingData <> " - " <> show allLogics
              return $
                Just $
                  CongestionChargeDetailsModel
                    { dpVersion = Nothing,
                      congestionChargePerMin = Nothing,
                      smartTipSuggestion = Nothing,
                      smartTipReason = Nothing,
                      congestionChargeMultiplier = Nothing,
                      congestionChargeData = FarePolicyD.CongestionChargeData {..},
                      driverExtraFeeBounds = Nothing,
                      ..
                    }
getCongestionChargeMultiplierFromModel' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = return Nothing

data CongestionChargeDetailsModel = CongestionChargeDetailsModel
  { dpVersion :: Maybe Text,
    mbSupplyDemandRatioToLoc :: Maybe Double,
    mbSupplyDemandRatioFromLoc :: Maybe Double,
    congestionChargePerMin :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    mbActualQARFromLocGeohash :: Maybe Double,
    mbActualQARCity :: Maybe Double,
    congestionChargeData :: FarePolicyD.CongestionChargeData,
    driverExtraFeeBounds :: Maybe DDriverExtraFeeBounds.DriverExtraFeeBounds,
    congestionChargeMultiplier :: Maybe FarePolicyD.CongestionChargeMultiplier
  }
  deriving (Generic, Show)

mkRainStatusKey :: Text -> Text
mkRainStatusKey geohash = "weather_union_" <> geohash

-- data GetCongestionChargeReq = GetCongestionChargeReq
--   { txnId :: Maybe Text,
--     fromLocation :: LatLong,
--     toLocation :: Maybe LatLong,
--     bookingTime :: Maybe UTCTime,
--     distance :: Meters,
--     duration :: Maybe Seconds,
--     merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
--     serviceTier :: ServiceTierType,
--     fare :: Maybe HighPrecMoney,
--     baseFare :: HighPrecMoney,
--     distanceFare :: Maybe HighPrecMoney,
--     pickupFare :: HighPrecMoney,
--     maxDAFare :: Maybe HighPrecMoney
--   }

-- data GetCongestionChargeRes = GetCongestionChargeRes
--   { congestionChargeMultiplier :: Maybe FarePolicyD.CongestionChargeMultiplier
--   }
