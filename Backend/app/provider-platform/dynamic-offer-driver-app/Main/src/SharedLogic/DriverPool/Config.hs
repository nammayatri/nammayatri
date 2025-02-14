{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.DriverPool.Config
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ But most likely you need a version from Cac with inMem results feature."
    #-}
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
import qualified Domain.Types as DVST
import Domain.Types.DriverPoolConfig as DDPC
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchTry
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Cac (CACData (..))
import Kernel.Types.Common
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import qualified Lib.Types.SpecialLocation as SL
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config (PoolSortingType (..))
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as CTC
import Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.DynamicLogic as TDL
import Utils.Common.CacUtils

getDriverPoolConfigFromDB' ::
  Maybe DVST.ServiceTierType ->
  String ->
  SL.Area ->
  Maybe Meters ->
  [DriverPoolConfig] ->
  Maybe DriverPoolConfig
getDriverPoolConfigFromDB' serviceTier tripCategory area mbDist configs = do
  let distance = fromMaybe 0 mbDist
  let mbApplicableConfig =
        find (filterByDistAndDvehAndArea serviceTier (Text.pack tripCategory) distance area) configs
          <|> find (filterByDistAndDvehAndArea serviceTier (Text.pack tripCategory) distance SL.Default) configs
  case configs of
    [] -> Nothing
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> Just applicableConfig
        Nothing -> do
          let alternativeConfigs =
                find (filterByDistAndDvehAndArea serviceTier "All" distance area) configs
                  <|> find (filterByDistAndDvehAndArea serviceTier "All" distance SL.Default) configs
          case alternativeConfigs of
            Just cfg -> Just cfg
            Nothing -> findDriverPoolConfig configs Nothing "All" distance area

filterByDistAndDvehAndArea :: Maybe DVST.ServiceTierType -> Text -> Meters -> SL.Area -> DriverPoolConfig -> Bool
filterByDistAndDvehAndArea serviceTier tripCategory dist area cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == serviceTier && cfg.tripCategory == tripCategory && cfg.area == area

findDriverPoolConfig :: [DriverPoolConfig] -> Maybe DVST.ServiceTierType -> Text -> Meters -> SL.Area -> Maybe DriverPoolConfig
findDriverPoolConfig configs serviceTier tripCategory dist area = do
  find (filterByDistAndDvehAndArea serviceTier tripCategory dist area) configs
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
        enableForwardBatching = True,
        enableUnifiedPooling = Just False,
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
        poolSortingType = Tagged, -- Example, replace with the actual default value
        radiusShrinkValueForDriversOnRide = Common.Meters 200,
        radiusStepSize = Common.Meters 50,
        scheduleTryTimes = [1, 2, 3],
        singleBatchProcessTime = Common.Seconds 120,
        thresholdToIgnoreActualDistanceThreshold = Nothing,
        timeBounds = Unbounded, -- Replace with actual default for `TimeBound`
        tripCategory = "DefaultTripCategory",
        tripDistance = Common.Meters 2000,
        updatedAt = read "2023-09-19 15:30:00 UTC",
        useOneToOneOsrmMapping = Nothing,
        vehicleVariant = Nothing,
        dynamicBatchSize = V.singleton 1
      }

getDriverPoolConfigFromDB ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    BeamFlow m r
  ) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  SL.Area ->
  Maybe Meters ->
  SearchRepeatType ->
  Int ->
  Maybe CacKey ->
  DSR.SearchRequest ->
  m (Maybe DriverPoolConfig)
getDriverPoolConfigFromDB merchantOpCityId serviceTier tripCategory area mbDist searchRepeatType searchRepeatCounter stickeyKey sreq = do
  configs <- CDP.findAllByMerchantOpCityIdInRideFlow merchantOpCityId sreq.configInExperimentVersions Nothing --- Rupak: Change this
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc -- bounds, all these params, timeDiffFromUTC
  let boundedConfigs = findBoundedDomain (filter (\cfg -> cfg.timeBounds /= Unbounded) configs) localTime
  let unboundedConfig = filter (\cfg -> cfg.timeBounds == Unbounded) configs
  let dpc' = getDriverPoolConfigFromDB' serviceTier tripCategory area mbDist boundedConfigs <|> getDriverPoolConfigFromDB' serviceTier tripCategory area mbDist unboundedConfig
  oldVersion <- getConfigVersion (getKeyValue <$> stickeyKey)
  (allLogics, version) <- TDL.getAppDynamicLogic (cast merchantOpCityId) (LYT.CONFIG LYT.DriverPoolConfig) localTime oldVersion Nothing
  let otherDimensions = A.Object $ KM.fromList [("serviceTier", toJSON serviceTier), ("tripCategory", toJSON tripCategory), ("area", toJSON area), ("tripDistance", toJSON mbDist), ("searchRepeatType", toJSON searchRepeatType), ("searchRepeatCounter", toJSON searchRepeatCounter)]
  case dpc' of
    Just dpc -> do
      let config = Config dpc (Just otherDimensions)
      resp <- LYTU.runLogics allLogics config
      case (fromJSON resp.result :: Result Config) of
        Success dpc'' -> do
          when
            (isJust version && isNothing oldVersion)
            ( do
                QSR.updatePoolingConfigVersion version sreq.id
                cacheConfigVersion (getKeyValue <$> stickeyKey) (fromJust version)
            )
          whenJust
            stickeyKey
            ( \stickeyKey' -> do
                let cacData = CACData (getKeyValue stickeyKey') (getKeyName stickeyKey') "" "driver_pool_config" (Text.pack (show version))
                fork "push driver_pool_config data to kafka" $ pushToKafka cacData "driver-pool-config-data" ""
            )
          pure $ Just dpc''.config
        A.Error e -> do
          logError $ "Error in applying dynamic logic: " <> show e
          incrementSystemConfigsFailedCounter "driver_pool_config_dynamic_logic_failure"
          pure dpc'
    Nothing -> pure Nothing

makeConfigVersionKey :: Text -> Text
makeConfigVersionKey id = "DRIVER_POOL_CONFIG:STKID" <> id

getConfigVersion :: (CacheFlow m r) => Maybe Text -> m (Maybe Int)
getConfigVersion mbStickyKey = do
  case mbStickyKey of
    Nothing -> pure Nothing
    Just stId' -> Hedis.withCrossAppRedis (Hedis.safeGet (makeConfigVersionKey stId'))

cacheConfigVersion :: (CacheFlow m r) => Maybe Text -> Int -> m ()
cacheConfigVersion mbStickyKey version = do
  expTime <- fromIntegral <$> asks (.cacConfig.cacExpTime)
  case mbStickyKey of
    Nothing -> pure ()
    Just stId' -> Hedis.withCrossAppRedis (Hedis.setExp (makeConfigVersionKey stId') version expTime)
