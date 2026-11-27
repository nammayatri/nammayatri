{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.DriverPool.Config
  ( module SharedLogic.DriverPool.Config,
    module CDP,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Default.Class
import Data.Text as Text hiding (filter, find)
import qualified Data.Vector as V
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.DriverPoolConfig as DDPC
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchTry
import Kernel.Prelude as KP
import Kernel.Storage.Clickhouse.Config
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Types.SpecialLocation
import qualified Lib.Types.SpecialLocation as SL
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import Lib.Yudhishthira.Tools.Utils (convertTags)
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.SystemConfigs ()
import Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import Storage.ConfigPilot.Config.DriverPoolConfig (DriverPoolConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.DynamicLogic as TDL
import qualified Tools.Fields as Fields

getDriverPoolConfigFromDB' ::
  Maybe DVST.ServiceTierType ->
  String ->
  SL.Area ->
  Maybe Meters ->
  [DriverPoolConfig] ->
  Maybe DriverPoolConfig
getDriverPoolConfigFromDB' serviceTier tripCategory area mbDist configs = do
  let distance = fromMaybe 0 mbDist
  let baseArea = SL.stripGateId area
      findWithFallback f =
        f area
          <|> (if SL.hasGateId area then f baseArea else Nothing)
          <|> f SL.Default
      mbApplicableConfig =
        findWithFallback (\a -> find (filterByDistAndDvehAndArea serviceTier (Text.pack tripCategory) distance a) configs)
  case configs of
    [] -> Nothing
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> Just applicableConfig
        Nothing -> do
          let alternativeConfigs =
                findWithFallback (\a -> find (filterByDistAndDvehAndArea serviceTier "All" distance a) configs)
          case alternativeConfigs of
            Just cfg -> Just cfg
            Nothing -> findDriverPoolConfig configs Nothing "All" distance area

filterByDistAndDvehAndArea :: Maybe DVST.ServiceTierType -> Text -> Meters -> SL.Area -> DriverPoolConfig -> Bool
filterByDistAndDvehAndArea serviceTier tripCategory dist area cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == serviceTier && cfg.tripCategory == tripCategory && cfg.area == area

findDriverPoolConfig :: [DriverPoolConfig] -> Maybe DVST.ServiceTierType -> Text -> Meters -> SL.Area -> Maybe DriverPoolConfig
findDriverPoolConfig configs serviceTier tripCategory dist area = do
  let baseArea = SL.stripGateId area
  find (filterByDistAndDvehAndArea serviceTier tripCategory dist area) configs
    <|> (if SL.hasGateId area then find (filterByDistAndDvehAndArea serviceTier tripCategory dist baseArea) configs else Nothing)
    <|> find (filterByDistAndDvehAndArea serviceTier tripCategory dist SL.Default) configs

data Config = Config
  { config :: DriverPoolConfig,
    extraDimensions :: Maybe Value
  }
  deriving (Generic, ToJSON, FromJSON, Show, Default)

-- instance Default Config where
--   def = Config def (Object KM.empty)

instance Default DriverPoolConfig where
  def =
    DDPC.DriverPoolConfig
      { actualDistanceThreshold = Nothing,
        actualDistanceThresholdOnRide = Nothing,
        area = Lib.Types.SpecialLocation.Default,
        batchSizeOnRide = 5,
        batchSizeOnRideWithStraightLineDistance = Nothing,
        createdAt = read "2023-09-18 12:34:56 UTC", -- Example timestamp, replace as needed
        currentRideTripCategoryValidForForwardBatching = ["TripCategory1", "TripCategory2"],
        distanceBasedBatchSplit = [],
        distanceUnit = Common.Kilometer,
        driverBatchSize = 3,
        driverPositionInfoExpiry = Nothing,
        driverQuoteLimit = 10,
        driverRequestCountLimit = 15,
        driverToDestinationDistanceThreshold = Common.Meters 1000,
        driverToDestinationDuration = Common.Seconds 900,
        enableDynamicPickupDistanceAndDuration = Nothing,
        enableEarlyBatchAdvanceOnFullReject = Nothing,
        batchingMode = Nothing,
        nextBatchScheduleTime = Nothing,
        enableForwardBatching = True,
        id = Id "default-driver-pool-config-id",
        maxDriverQuotesRequired = 5,
        maxNumberOfBatches = 4,
        maxParallelSearchRequests = 10,
        maxParallelSearchRequestsOnRide = 5,
        maxRadiusOfSearch = Common.Meters 5000,
        merchantId = Id "default-merchant-id",
        merchantOperatingCityId = Id "default-city-id",
        minRadiusOfSearch = Common.Meters 500,
        onRideBatchSplitConfig = [], -- Define default value
        onRideRadiusConfig = [], -- Define default value
        radiusShrinkValueForDriversOnRide = Common.Meters 200,
        radiusStepSize = Nothing,
        scheduleTryTimes = [1, 2, 3],
        singleBatchProcessTime = Common.Seconds 120,
        softMaxParallelSearchRequests = Nothing,
        autoAssignMaxPickupDistance = Nothing,
        thresholdToIgnoreActualDistanceThreshold = Nothing,
        timeBounds = Unbounded, -- Replace with actual default for `TimeBound`
        tripCategory = "DefaultTripCategory",
        tripDistance = Common.Meters 2000,
        updatedAt = read "2023-09-19 15:30:00 UTC",
        useOneToOneOsrmMapping = Nothing,
        vehicleVariant = Nothing,
        dynamicBatchSize = V.singleton 1,
        selfRequestIfRiderIsDriver = False,
        enablePriorityTagSplit = Nothing,
        srdCountersBulkChunkSize = Nothing,
        idleBulkChunkSize = Nothing
      }

getDriverPoolConfig ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    BeamFlow m r,
    ClickhouseFlow m r
  ) =>
  Id MerchantOperatingCity ->
  DVST.ServiceTierType ->
  DTC.TripCategory ->
  SL.Area ->
  Maybe Meters ->
  SearchRepeatType ->
  Int ->
  DSR.SearchRequest ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId serviceTier tripCategory area tripDistance searchRepeatType searchRepeatCounter sreq = do
  config <-
    getDriverPoolConfigFromDB merchantOpCityId (Just serviceTier) (show tripCategory) area tripDistance searchRepeatType searchRepeatCounter sreq
      |<|>| getDriverPoolConfigFromDB merchantOpCityId (Just serviceTier) "All" area tripDistance searchRepeatType searchRepeatCounter sreq
  when (isNothing config) do
    logError $ "Could not find the config for merchantOpCityId:" <> getId merchantOpCityId <> " and serviceTier:" <> show serviceTier <> " and tripCategory:" <> show tripCategory <> " and tripDistance:" <> show tripDistance
    throwError $ InvalidRequest $ "DriverPool Configs not found for MerchantOperatingCity: " <> merchantOpCityId.getId
  logDebug $ "driverPoolConfig we recieved for merchantOpCityId:" <> getId merchantOpCityId <> " and serviceTier:" <> show serviceTier <> " and tripCategory:" <> show tripCategory <> " and tripDistance:" <> show tripDistance <> " is:" <> show config
  pure (fromJust config)

getDriverPoolConfigFromDB ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    BeamFlow m r,
    ClickhouseFlow m r
  ) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  SL.Area ->
  Maybe Meters ->
  SearchRepeatType ->
  Int ->
  DSR.SearchRequest ->
  m (Maybe DriverPoolConfig)
getDriverPoolConfigFromDB merchantOpCityId serviceTier tripCategory area mbDist searchRepeatType searchRepeatCounter sreq = do
  configs <- getConfig (DriverPoolConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, tripDistance = Nothing, area = Nothing, vehicleVariant = Nothing, tripCategory = Nothing}) (Just (CDP.findAllByMerchantOpCityIdInRideFlow merchantOpCityId sreq.configInExperimentVersions Nothing))
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc -- bounds, all these params, timeDiffFromUTC
  let boundedConfigs = findBoundedDomain (filter (\cfg -> cfg.timeBounds /= Unbounded) configs) localTime
  let unboundedConfig = filter (\cfg -> cfg.timeBounds == Unbounded) configs
  let dpc' = getDriverPoolConfigFromDB' serviceTier tripCategory area mbDist boundedConfigs <|> getDriverPoolConfigFromDB' serviceTier tripCategory area mbDist unboundedConfig
      oldVersion = sreq.poolingConfigVersion
  (allLogics, version) <- TDL.getAppDynamicLogic (cast merchantOpCityId) (LYT.CONFIG LYT.DriverPoolConfig) localTime oldVersion Nothing
  let customerTags = convertTags <$> sreq.customerNammaTags
  now <- getCurrentTime
  let timeToPickupSeconds = round (diffUTCTime sreq.startTime now) :: Int
  let otherDimensions = A.Object $ KM.fromList [("serviceTier", toJSON serviceTier), ("tripCategory", toJSON tripCategory), ("area", toJSON area), ("tripDistance", toJSON mbDist), ("searchRepeatType", toJSON searchRepeatType), ("searchRepeatCounter", toJSON searchRepeatCounter), ("customerTags", toJSON customerTags), ("timeToPickupSeconds", toJSON timeToPickupSeconds)]
  case dpc' of
    Just dpc -> do
      let config = Config dpc (Just otherDimensions)
      resp <- LYDL.runLogicsWithDebugLog LYDL.Driver (cast merchantOpCityId) (LYT.CONFIG LYT.DriverPoolConfig) (Just sreq.transactionId) allLogics config
      case (fromJSON resp.result :: Result Config) of
        Success dpc'' -> do
          when (isJust version && isNothing oldVersion) $
            QSR.updatePoolingConfigVersion version sreq.id
          pure $ Just dpc''.config
        A.Error e -> do
          logError $ "Error in applying dynamic logic: " <> show e
          incrementSystemConfigsFailedCounter "driver_pool_config_dynamic_logic_failure"
          pure dpc'
    Nothing -> pure Nothing
