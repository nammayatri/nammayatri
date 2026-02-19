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
import Data.Text as T hiding (elem, find, null)
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
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleServiceTier as DVST
import GHC.Float (int2Double)
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
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
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.CancellationFarePolicy as QCCFP
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Tools.DynamicLogic
import Tools.Error
import Tools.Maps
import Utils.Common.Cac.KeyNameConstants

data FarePoliciesProduct = FarePoliciesProduct
  { farePolicies :: [FarePolicyD.FullFarePolicy],
    area :: SL.Area,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text
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

getFarePolicyByEstOrQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m FarePolicyD.FullFarePolicy
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

getFarePolicyOnEndRide :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> LatLong -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe UTCTime -> Maybe Bool -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicyOnEndRide mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration toLocationLatLong merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId mbBookingStartTime isDashboardRequest mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName = do
  selectedFarePolicy <- case area of
    Just SL.Default -> handleFarePolicy
    Just (SL.Drop _) -> handleFarePolicy
    _ -> getFarePolicyByEstOrQuoteId mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId mbBookingStartTime isDashboardRequest mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName
  return selectedFarePolicy
  where
    handleFarePolicy = do
      dropSpecialLocation <- Esq.runInReplica (QSpecialLocation.findSpecialLocationByLatLong' toLocationLatLong) >>= mapM (FareProduct.getDropSpecialLocation merchantOpCityId)
      let isDropSpecialLocation = (Just . SL.Drop . DSpecialLocation.id . fst) =<< dropSpecialLocation
          dropSpecialLocName = (.locationName) . fst <$> dropSpecialLocation
      selectedFarePolicy' <-
        if isJust isDropSpecialLocation && area /= isDropSpecialLocation
          then getFarePolicy mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId (fromMaybe False isDashboardRequest) tripCategory vehicleServiceTier isDropSpecialLocation mbBookingStartTime mbAppDynamicLogicVersion txnId configInExperimentVersions dropSpecialLocName
          else getFarePolicyByEstOrQuoteId mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId mbBookingStartTime isDashboardRequest mbAppDynamicLogicVersion txnId configInExperimentVersions mbSpecialLocName
      logInfo $ "Drop Special Location during end ride: " <> show isDropSpecialLocation <> " and area at estimate stage is: " <> show area <> ", fare policyId for end ride calc: " <> show (selectedFarePolicy'.id)
      return selectedFarePolicy'

getFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> Bool -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Maybe UTCTime -> Maybe Int -> Maybe CacKey -> [LYT.ConfigVersionMap] -> Maybe Text -> m FarePolicyD.FullFarePolicy
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
      mbVehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow serviceTier merchantOpCityId configsInExperimentVersions
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and findByServiceTierTypeAndCityId : " <> show mbVehicleServiceTierItem
      (mbBaseVariantCarFareProduct :: Maybe FareProduct.FareProduct) <- case mbVehicleServiceTierItem of
        Just vehicleServiceTierItem -> do
          if vehicleServiceTierItem.vehicleCategory == Just DVC.CAR
            then do
              maybe (return Nothing) (\vehicleServiceTier -> getFareProduct' areaName vehicleServiceTier.serviceTierType)
                =<< CQVST.findBaseServiceTierTypeByCategoryAndCityIdInRideFlow (Just DVC.CAR) merchantOpCityId configsInExperimentVersions
            else return Nothing
        Nothing -> return Nothing
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and mbBaseVariantCarFareProduct : " <> show mbBaseVariantCarFareProduct
      baseVariantFareAmountCar <- getBaseVariantFarePolicy mbFromlocaton mbFromlocaton merchantOpCityId mbBaseVariantCarFareProduct txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion configsInExperimentVersions mbSpecialLocName
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and baseVariantFareAmountCar : " <> show baseVariantFareAmountCar
      fp <- getFullFarePolicy mbFromlocaton mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId mbBookingStartTime baseVariantFareAmountCar mbAppDynamicLogicVersion mbSpecialLocName fareProduct configsInExperimentVersions
      logInfo $ "Dynamic Pricing debugging getFarePolicyWithArea txnId: " <> show txnId <> " and getFullFarePolicy : " <> show fp
      return fp

getAllFarePoliciesProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Id Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> LatLong -> Maybe LatLong -> Maybe (Id SL.SpecialLocation) -> Maybe (Id SL.SpecialLocation) -> Maybe CacKey -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Maybe Int -> DTC.TripCategory -> [LYT.ConfigVersionMap] -> m FarePoliciesProduct
getAllFarePoliciesProduct merchantId merchantOpCityId isDashboard fromlocaton mbToLocation mbFromSpecialLocationId mbToSpecialLocationId txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion tripCategory configsInExperimentVersions = do
  let searchSources = FareProduct.getSearchSources isDashboard
  allFareProducts <- FareProduct.getAllFareProducts merchantId merchantOpCityId searchSources fromlocaton mbToLocation mbFromSpecialLocationId mbToSpecialLocationId tripCategory
  (mbBaseVariantCarFareProduct :: Maybe FareProduct.FareProduct) <-
    return . getFareProduct allFareProducts
      =<< CQVST.findBaseServiceTierTypeByCategoryAndCityIdInRideFlow (Just DVC.CAR) merchantOpCityId configsInExperimentVersions
  baseVariantFareAmountCar <- getBaseVariantFarePolicy (Just fromlocaton) mbToLocation merchantOpCityId mbBaseVariantCarFareProduct txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion configsInExperimentVersions allFareProducts.specialLocationName
  farePolicies <- mapM (\fareProduct -> getFullFarePolicy (Just fromlocaton) mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId Nothing baseVariantFareAmountCar mbAppDynamicLogicVersion allFareProducts.specialLocationName fareProduct configsInExperimentVersions) allFareProducts.fareProducts
  return $
    FarePoliciesProduct
      { farePolicies,
        area = allFareProducts.area,
        specialLocationTag = allFareProducts.specialLocationTag,
        specialLocationName = allFareProducts.specialLocationName
      }

getBaseVariantFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Maybe LatLong -> Maybe LatLong -> Id DMOC.MerchantOperatingCity -> Maybe FareProduct.FareProduct -> Maybe CacKey -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Maybe Int -> [LYT.ConfigVersionMap] -> Maybe Text -> m (Maybe HighPrecMoney)
getBaseVariantFarePolicy mbFromLocation mbToLocation merchantOpCityId mbBaseVariantCarFareProduct txnId mbFromLocGeohash mbToLocGeohash mbDistance mbDuration mbAppDynamicLogicVersion configsInExperimentVersions mbSpecialLocName = do
  mbBaseVariantFarePolicy <- traverse (\fareProduct -> getFullFarePolicy mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId Nothing Nothing mbAppDynamicLogicVersion mbSpecialLocName fareProduct configsInExperimentVersions) mbBaseVariantCarFareProduct
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

getFullFarePolicy :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Maybe LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> Maybe Meters -> Maybe Seconds -> Maybe CacKey -> Maybe UTCTime -> Maybe HighPrecMoney -> Maybe Int -> Maybe Text -> FareProduct.FareProduct -> [LYT.ConfigVersionMap] -> m FarePolicyD.FullFarePolicy
getFullFarePolicy mbFromLocation mbToLocation mbFromLocGeohash mbToLocGeohash mbDistance mbDuration txnId mbBookingStartTime mbBaseVaraintCarPrice mbAppDynamicLogicVersion mbSpecialLocName fareProduct configsInExperimentVersions = do
  transporterConfig <- CTC.findByMerchantOpCityId fareProduct.merchantOperatingCityId txnId >>= fromMaybeM (TransporterConfigNotFound fareProduct.merchantOperatingCityId.getId)
  mbVehicleServiceTierItem <-
    CQVST.findByServiceTierTypeAndCityIdInRideFlow fareProduct.vehicleServiceTier fareProduct.merchantOperatingCityId configsInExperimentVersions
  let whiteListedGeohashes = fromMaybe [] transporterConfig.dpWhiteListedGeohash
      blackListedGeohashes = fromMaybe [] transporterConfig.dpBlackListedGeohash
  now <- getCurrentTime
  let _bookingStartTime = fromMaybe now mbBookingStartTime
  let localTimeZoneSeconds = transporterConfig.timeDiffFromUtc
  congestionChargeMultiplierFromModel <-
    case fareProduct.tripCategory of
      DTC.OneWay v | v /= MeterRide -> do
        maybe (return Nothing) (checkGeoHashAndCalculate mbVehicleServiceTierItem localTimeZoneSeconds whiteListedGeohashes blackListedGeohashes transporterConfig mbFromLocGeohash) mbFromLocation
      _ -> return Nothing -- For now, we are not supporting congestion charge through model for other trips
  farePolicy' <- QFP.findById txnId fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
  cancellationFarePolicy <- maybe (return Nothing) QCCFP.findById farePolicy'.cancellationFarePolicyId
  mlCongestionChargeMultiplier <-
    if transporterConfig.isMLBasedDynamicPricingEnabled
      then calculateCongestionChargeViaML mbDistance mbFromLocation cancellationFarePolicy farePolicy' Nothing
      else return Nothing
  let (updatedCongestionChargePerMin, updatedCongestionChargeMultiplier, version, supplyDemandRatioFromLoc, supplyDemandRatioToLoc, smartTipSuggestion, smartTipReason, mbActualQARFromLocGeohash, mbActualQARCity, mbcongestionChargeData, mbDriverExtraFeeBounds) =
        case congestionChargeMultiplierFromModel of
          Just details ->
            case (details.congestionChargePerMin, details.smartTipSuggestion, details.congestionChargeMultiplier) of
              (Just congestionChargePerMinute, smartTip, Nothing) -> (Just congestionChargePerMinute, Nothing, details.dpVersion, details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, smartTip, details.smartTipReason, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds) -----------Need to send Nothing here for congestionChargeMultiplier
              (Nothing, smartTip, Just congestionChargeMultiplier) -> (Nothing, mlCongestionChargeMultiplier <|> Just congestionChargeMultiplier, if isJust mlCongestionChargeMultiplier then Just "ML" else details.dpVersion, details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, smartTip, details.smartTipReason, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds) -----------Need to send Nothing here for congestionChargePerMinute
              (Nothing, Just smartTip, Nothing) -> (Nothing, mlCongestionChargeMultiplier <|> farePolicy'.congestionChargeMultiplier, if isJust mlCongestionChargeMultiplier then Just "ML" else details.dpVersion, details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, Just smartTip, details.smartTipReason, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds)
              _ -> (Nothing, mlCongestionChargeMultiplier <|> farePolicy'.congestionChargeMultiplier, if isJust mlCongestionChargeMultiplier then Just "ML" else Just "Static", details.mbSupplyDemandRatioFromLoc, details.mbSupplyDemandRatioToLoc, Nothing, Nothing, details.mbActualQARFromLocGeohash, details.mbActualQARCity, Just details.congestionChargeData, details.driverExtraFeeBounds)
          Nothing -> (Nothing, farePolicy'.congestionChargeMultiplier, Just "Static", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
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
          -- estimatedFare <- calculateFareForFarePolicy fullFarePolicy mbDistance mbDuration fareProduct.merchantOperatingCityId
          if fareWithAddition > estimatedFare
            then do
              logWarning $ "Fare with addition: " <> show fareWithAddition <> " is greater than estimated fare: " <> show estimatedFare
              let fullFarePolicy' = fullFarePolicy{additionalCongestionCharge = fareWithAddition - estimatedFare}
              return fullFarePolicy'
            else return fullFarePolicy
        else return fullFarePolicy
    Nothing -> return fullFarePolicy
  where
    checkGeoHashAndCalculate mbVehicleServiceTierItem localTimeZoneSeconds whiteListedGeohashes blackListedGeohashes transporterConfig mbfromLocGeohash fromLocation = do
      let fromLocGeohash = fromMaybe (fromMaybe "" $ T.pack <$> Geohash.encode (fromMaybe 5 transporterConfig.dpGeoHashPercision) (fromLocation.lat, fromLocation.lon)) mbfromLocGeohash
      if elem fromLocGeohash whiteListedGeohashes || notElem fromLocGeohash blackListedGeohashes
        then do
          logInfo $ "Calling DynamicPricing 1" <> show localTimeZoneSeconds <> show fromLocGeohash <> show mbToLocGeohash <> show fareProduct.vehicleServiceTier <> show mbDistance <> show mbDuration <> show transporterConfig.isDynamicPricingQARCalEnabled <> show transporterConfig.qarCalRadiusInKm <> show mbAppDynamicLogicVersion <> show fareProduct.merchantOperatingCityId
          getCongestionChargeMultiplierFromModel' localTimeZoneSeconds (Just fromLocation) (Just fromLocGeohash) mbToLocGeohash fareProduct.vehicleServiceTier (maybe Nothing (.vehicleCategory) mbVehicleServiceTierItem) mbDistance mbDuration transporterConfig.isDynamicPricingQARCalEnabled transporterConfig.qarCalRadiusInKm mbSpecialLocName mbAppDynamicLogicVersion fareProduct.merchantOperatingCityId
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

calculateFareParametersForFarePolicy :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => FarePolicyD.FullFarePolicy -> Maybe Meters -> Maybe Seconds -> Id DMOC.MerchantOperatingCity -> m DFareParameters.FareParameters
calculateFareParametersForFarePolicy fullFarePolicy mbDistance mbDuration merchantOperatingCityId = do
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOperatingCityId
  distanceUnit <- SMerchant.getDistanceUnitByMerchantOpCity merchantOperatingCityId
  now <- getCurrentTime
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
            numberOfLuggages = Nothing
          }
  SFC.calculateFareParameters params

mkFarePolicyBreakups :: (Text -> breakupItemValue) -> (Text -> breakupItemValue -> breakupItem) -> Maybe Meters -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> FarePolicyD.FarePolicy -> [breakupItem]
mkFarePolicyBreakups mkValue mkBreakupItem mbDistance mbCancellationCharge mbTollCharges estimatedTotalFare congestionChargeViaDp farePolicy = do
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
      governmentChargeItem = mkBreakupItem governmentChargeCaption . (mkValue . show) <$> farePolicy.govtCharges

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

      additionalDetailsBreakups = processAdditionalDetails farePolicy.farePolicyDetails
      conditionalCharges = processAdditionalCharges farePolicy.conditionalCharges
  catMaybes
    [ tollChargesItem,
      serviceChargeItem,
      cancellationChargeItem,
      parkingChargeItem,
      governmentChargeItem,
      driverMinExtraFeeItem,
      businessDiscountItem,
      personalDiscountItem,
      driverMaxExtraFeeItem,
      petChargesItem,
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
      returnFeePercentageItem
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

getCongestionChargeMultiplierFromModel' ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  ) =>
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
  m (Maybe CongestionChargeDetailsModel)
getCongestionChargeMultiplierFromModel' timeDiffFromUtc (Just fromLocation) (Just fromLocGeohash) toLocGeohash serviceTier vehicleCategory (Just (Meters distance)) (Just (Seconds duration)) (Just True) radius' mbSpecialLocName (Just dynamicPricingLogicVersion) merchantOperatingCityId = do
  localTime <- getLocalCurrentTime timeDiffFromUtc
  logInfo $ "Calling DynamicPricing" <> show fromLocGeohash
  now <- getCurrentTime
  let radius = fromMaybe 3.0 radius'
  let distanceInKm = int2Double distance / 1000.0
  let distanceBin = getDistanceBin distance
  let estimatedDurationInH = int2Double duration / 3600.0
  let pastTimeInterval = addUTCTime (-900) now
  let speedKmh = if estimatedDurationInH == 0.0 then 0.0 else distanceInKm / estimatedDurationInH
  toss <- getRandomInRange (1, 100 :: Int)
  mbSupplyDemandRatioFromLoc <- Hedis.withCrossAppRedis $ Hedis.get $ mkSupplyDemandRatioKeyWithGeohash fromLocGeohash vehicleCategory
  -- mbActualQARFromLocGeohashDistance <- Hedis.withCrossAppRedis $ Hedis.get $ mkActualQARKeyWithGeohashAndDistanceBin fromLocGeohash distanceBin vehicleCategory
  mbActualQARFromLocGeohashDistance <- getQARWithDistance now vehicleCategory distance radius fromLocation
  -- mbActualQARFromLocGeohash <- Hedis.withCrossAppRedis $ Hedis.get $ mkActualQARKeyWithGeohash fromLocGeohash vehicleCategory
  mbActualQARFromLocGeohash <- getQARVehicleCategory now vehicleCategory radius fromLocation
  mbActualQARFromLocGeohash2xRadius <- getQARVehicleCategory now vehicleCategory (2 * radius) fromLocation
  mbActualQARCity <- getQARVehicleCategoryCity now vehicleCategory merchantOperatingCityId.getId
  -- mbActualQARCity <- Hedis.withCrossAppRedis $ Hedis.get $ mkActualQARKeyWithCity merchantOperatingCityId.getId vehicleCategory
  -- mbActualQARFromLocGeohashDistancePast <- Hedis.withCrossAppRedis $ Hedis.get $ mkActualQARKeyWithGeohashAndDistanceBinPast fromLocGeohash distanceBin vehicleCategory
  -- mbActualQARFromLocGeohashPast <- Hedis.withCrossAppRedis $ Hedis.get $ mkActualQARKeyWithGeohashPast fromLocGeohash vehicleCategory
  -- mbActualQARCityPast <- Hedis.withCrossAppRedis $ Hedis.get $ mkActualQARKeyWithCityPast merchantOperatingCityId.getId vehicleCategory
  mbActualQARFromLocGeohashDistancePast <- getQARWithDistance pastTimeInterval vehicleCategory distance radius fromLocation
  mbActualQARFromLocGeohashPast <- getQARVehicleCategory pastTimeInterval vehicleCategory radius fromLocation
  mbActualQARCityPast <- getQARVehicleCategoryCity pastTimeInterval vehicleCategory merchantOperatingCityId.getId
  mbCongestionFromLocGeohash <- Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohash fromLocGeohash
  mbCongestionFromLocGeohashDistance <- Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohashAndDistanceBin fromLocGeohash distanceBin
  mbCongestionCity <- Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithCity merchantOperatingCityId.getId
  mbCongestionFromLocGeohashPast <- Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohashPast fromLocGeohash
  (mbRainStatus :: Maybe Text) <- Hedis.withCrossAppRedis $ Hedis.get $ mkRainStatusKey fromLocGeohash
  logInfo $ "Calling DynamicPricing" <> show mbRainStatus
  mbCongestionFromLocGeohashDistancePast <- Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithGeohashAndDistanceBinPast fromLocGeohash distanceBin
  mbCongestionCityPast <- Hedis.withCrossAppRedis $ Hedis.get $ mkCongestionKeyWithCityPast merchantOperatingCityId.getId
  let actualQAR = mbActualQARFromLocGeohashDistance <|> mbActualQARFromLocGeohash <|> mbActualQARFromLocGeohash2xRadius <|> mbActualQARCity
  let actualQARPast = mbActualQARFromLocGeohashDistancePast <|> mbActualQARFromLocGeohashPast <|> mbActualQARCityPast
  let congestionMultiplier = mbCongestionFromLocGeohashDistance <|> mbCongestionFromLocGeohash <|> mbCongestionCity
  let congestionMultiplierPast = mbCongestionFromLocGeohashDistancePast <|> mbCongestionFromLocGeohashPast <|> mbCongestionCityPast
  mbSupplyDemandRatioToLoc <- join <$> traverse (\locgeohash -> Hedis.withCrossAppRedis $ Hedis.get $ mkSupplyDemandRatioKeyWithGeohash locgeohash vehicleCategory) toLocGeohash
  (allLogics, mbVersion) <- getAppDynamicLogic (cast merchantOperatingCityId) LYT.DYNAMIC_PRICING_UNIFIED localTime (Just dynamicPricingLogicVersion) Nothing
  let dynamicPricingData = DynamicPricingData {serviceTier, speedKmh, distanceInKm, supplyDemandRatioFromLoc = fromMaybe 0.0 mbSupplyDemandRatioFromLoc, supplyDemandRatioToLoc = fromMaybe 0.0 mbSupplyDemandRatioToLoc, toss, actualQAR = actualQAR, actualQARPast = actualQARPast, congestionMultiplier = congestionMultiplier, congestionMultiplierPast = congestionMultiplierPast, rainStatus = mbRainStatus, mbSpecialLocName = mbSpecialLocName}
  if null allLogics
    then do
      logInfo $ "No DynamicPricingLogics found for merchantOperatingCityId : " <> show merchantOperatingCityId <> " and serviceTier : " <> show serviceTier <> " and localTime : " <> show localTime
      return Nothing
    else do
      response <- withTryCatch "runLogics:getCongestionChargeMultiplierFromModel" $ LYDL.runLogicsWithDebugLog (cast merchantOperatingCityId) LYT.DYNAMIC_PRICING_UNIFIED allLogics dynamicPricingData
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
getCongestionChargeMultiplierFromModel' _ _ _ _ _ _ _ _ _ _ _ _ _ = return Nothing

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
