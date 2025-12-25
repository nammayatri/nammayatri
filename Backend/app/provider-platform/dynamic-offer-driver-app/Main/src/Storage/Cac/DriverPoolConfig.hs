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

module Storage.Cac.DriverPoolConfig (module Storage.Cac.DriverPoolConfig, module Reexport) where

import qualified Client.Main as CM
import Data.Aeson as DA
import Data.Text as Text hiding (find)
import Data.Time
import Data.Time.Calendar.WeekDate
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.Cac as DTC
import Domain.Types.DriverPoolConfig
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Kernel.Beam.Functions as KBF
import Kernel.Prelude as KP
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, getLocalCurrentTime, utcTimeToDiffTime)
import Kernel.Utils.Error
import Kernel.Utils.Logging
import qualified Lib.Types.SpecialLocation as SL
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.DriverPool.Config as DPC
import SharedLogic.DriverPool.Types as Reexport
import qualified Storage.Beam.DriverPoolConfig as SBMDPC
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import Storage.Queries.DriverPoolConfig ()
import Utils.Common.CacUtils as CCU

getSearchDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r, BeamFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Meters ->
  SL.Area ->
  DSR.SearchRequest ->
  m (Maybe DriverPoolConfig)
getSearchDriverPoolConfig merchantOpCityId mbDist area sreq = do
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let currTimeOfDay = round (realToFrac (utcTimeToDiffTime localTime) :: Double)
      currentDay = utctDay localTime
      (_, _, currentDayOfWeek) = toWeekDate currentDay
      serviceTier = Nothing
      tripCategory = "All"
  getDriverPoolConfigCond merchantOpCityId serviceTier tripCategory mbDist area DST.INITIAL 0 Nothing (Just currTimeOfDay) (Just currentDayOfWeek) sreq
    |<|>| getDriverPoolConfigCond merchantOpCityId serviceTier tripCategory mbDist area DST.INITIAL 0 Nothing Nothing Nothing sreq

getDriverPoolConfigFromCAC ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Meters ->
  SL.Area ->
  Maybe CacKey ->
  Maybe Int ->
  Maybe Int ->
  m (Maybe DriverPoolConfig)
getDriverPoolConfigFromCAC merchantOpCityId st tc dist area stickyKey currTimeOfDay currentDayOfWeek = do
  let dpcCond =
        [ (MerchantOperatingCityId, toJSON (getId merchantOpCityId)),
          (TripDistance, toJSON (getMeters dist)),
          (TripCategory, toJSON tc),
          (Area, show area)
        ]
          <> [(VehicleVariant, show (fromJust st)) | isJust st]
          <> [(CCU.TimeOfDay, show (fromJust currTimeOfDay)) | isJust currTimeOfDay]
          <> [(DayOfWeek, show (fromJust currentDayOfWeek)) | isJust currentDayOfWeek]
  inMemConfig <- getConfigFromInMemory merchantOpCityId st tc dist
  config <- CCU.getConfigFromCacOrDB inMemConfig dpcCond stickyKey (KBF.fromCacType @SBMDPC.DriverPoolConfig) CCU.DriverPoolConfig
  whenJust config $ pure $ void $ setConfigInMemory merchantOpCityId st tc dist config
  bool
    (pure Nothing)
    (pure config)
    ( (merchantOperatingCityId <$> config) == Just merchantOpCityId && (vehicleVariant <$> config) == Just st
        && (Domain.Types.DriverPoolConfig.tripCategory <$> config) == Just (Text.pack tc)
        && (Domain.Types.DriverPoolConfig.area <$> config) == Just area
    )

doubleToInt :: Double -> Int
doubleToInt = floor

getDriverPoolConfigCond ::
  (CacheFlow m r, EsqDBFlow m r, BeamFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Maybe Meters ->
  SL.Area ->
  DST.SearchRepeatType ->
  Int ->
  Maybe CacKey ->
  Maybe Int ->
  Maybe Int ->
  DSR.SearchRequest ->
  m (Maybe DriverPoolConfig)
getDriverPoolConfigCond merchantOpCityId serviceTier tripCategory dist' area searchRepeatType searchRepeatCounter stickeyKey currTimeOfDay currentDayOfWeek sreq = do
  let dist = fromMaybe 0 dist'
  getDriverPoolConfigFromCAC merchantOpCityId serviceTier tripCategory dist area stickeyKey currTimeOfDay currentDayOfWeek
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId serviceTier tripCategory dist SL.Default stickeyKey currTimeOfDay currentDayOfWeek
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId serviceTier "All" dist area stickeyKey currTimeOfDay currentDayOfWeek
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId serviceTier "All" dist SL.Default stickeyKey currTimeOfDay currentDayOfWeek
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId Nothing "All" dist area stickeyKey currTimeOfDay currentDayOfWeek
    |<|>| getDriverPoolConfigFromCAC merchantOpCityId Nothing "All" dist SL.Default stickeyKey currTimeOfDay currentDayOfWeek
    |<|>| ( do
              logDebug $ "DriverPoolConfig not found in memory, fetching from DB for context: " <> show (merchantOpCityId, serviceTier, tripCategory, dist, area)
              DPC.getDriverPoolConfigFromDB merchantOpCityId serviceTier tripCategory area dist' searchRepeatType searchRepeatCounter stickeyKey sreq
          )

-- TODO :: Need To Handle `area` Properly In CAC
getConfigFromInMemory ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Meters ->
  m (Maybe DriverPoolConfig)
getConfigFromInMemory id mbvst tripCategory dist = do
  let roundeDist = doubleToInt (fromIntegral (dist.getMeters) / 1000)
  isExpired <- DTC.updateConfig DTC.LastUpdatedDriverPoolConfig
  getConfigFromMemoryCommon (DTC.DriverPoolConfig id.getId (show mbvst) tripCategory roundeDist) isExpired CM.isExperimentsRunning

setConfigInMemory ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  Meters ->
  Maybe DriverPoolConfig ->
  m (Maybe DriverPoolConfig)
setConfigInMemory id mbvst tripCategory dist config = do
  let roundeDist = doubleToInt (fromIntegral (dist.getMeters) / 1000)
  isExp <- DTC.inMemConfigUpdateTime DTC.LastUpdatedDriverPoolConfig
  CCU.setConfigInMemoryCommon (DTC.DriverPoolConfig id.getId (show mbvst) tripCategory roundeDist) isExp config

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r, BeamFlow m r) =>
  Id MerchantOperatingCity ->
  DVST.ServiceTierType ->
  DTC.TripCategory ->
  SL.Area ->
  Maybe Meters ->
  DST.SearchRepeatType ->
  Int ->
  Maybe CacKey ->
  DSR.SearchRequest ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId serviceTier tripCategory area tripDistance searchRepeatType searchRepeatCounter srId sreq = do
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let currTimeOfDay = round (realToFrac (utcTimeToDiffTime localTime) :: Double)
      currentDay = utctDay localTime
      (_, _, currentDayOfWeek) = toWeekDate currentDay
  config <-
    getDriverPoolConfigCond merchantOpCityId (Just serviceTier) (show tripCategory) tripDistance area searchRepeatType searchRepeatCounter srId (Just currTimeOfDay) (Just currentDayOfWeek) sreq
      |<|>| getDriverPoolConfigCond merchantOpCityId (Just serviceTier) "All" tripDistance area searchRepeatType searchRepeatCounter srId Nothing Nothing sreq
  when (isNothing config) do
    logError $ "Could not find the config for merchantOpCityId:" <> getId merchantOpCityId <> " and serviceTier:" <> show serviceTier <> " and tripCategory:" <> show tripCategory <> " and tripDistance:" <> show tripDistance
    throwError $ InvalidRequest $ "DriverPool Configs not found for MerchantOperatingCity: " <> merchantOpCityId.getId
  logDebug $ "driverPoolConfig we recieved for merchantOpCityId:" <> getId merchantOpCityId <> " and serviceTier:" <> show serviceTier <> " and tripCategory:" <> show tripCategory <> " and tripDistance:" <> show tripDistance <> " is:" <> show config
  pure (fromJust config)

----------------------------------------------- Cached Queries Can't be handled by cac ------------------------------------------

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
create = CDP.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> Maybe Value -> m [DriverPoolConfig]
findAllByMerchantOpCityId = CDP.findAllByMerchantOpCityId

findByMerchantOpCityIdAndTripDistance :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe [LYT.ConfigVersionMap] -> Maybe Value -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistance = CDP.findByMerchantOpCityIdAndTripDistance

findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe DVST.ServiceTierType -> Text -> SL.Area -> Maybe [LYT.ConfigVersionMap] -> Maybe Value -> m (Maybe DriverPoolConfig)
findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh = CDP.findByMerchantOpCityIdAndTripDistanceAndAreaAndDVeh

-- Call it after any update
clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCache = CDP.clearCache

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverPoolConfig -> m ()
update = CDP.update

instance FromCacType SBMDPC.DriverPoolConfig DriverPoolConfig where
  fromCacType = fromTType'
