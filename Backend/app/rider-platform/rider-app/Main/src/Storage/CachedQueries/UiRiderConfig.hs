{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.UiRiderConfig where

import qualified Data.Text as Text
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.UiRiderConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version (DeviceType)
import Kernel.Utils.Common (CacheFlow, throwError)
-- import Kernel.Utils.Time
-- import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as YType
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.UiRiderConfig as Queries
import Tools.Error

-- import qualified Tools.DynamicLogic as TDL

findUiConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => YType.UiConfigRequest -> Id MerchantOperatingCity -> Bool -> m (Maybe UiRiderConfig, Maybe Int)
findUiConfig _ _ _ = do
  throwError $ InvalidRequest $ "UI config not supported yet"

-- localTime <- getLocalCurrentTime 19800 -- Fix Me
-- version <- TDL.selectAppDynamicLogicVersion (cast merchantOperatingCityId) (LYT.RIDER_CONFIG (LYT.UiConfig os platform)) localTime toss >>= fromMaybeM (InvalidRequest $ "No version found. No dynamic logic found for merchantOpCityId: " <> show merchantOperatingCityId <> " and domain: " <> show (LYT.RIDER_CONFIG (LYT.UiConfig os platform)))
-- let mbConfigInExperimentVersions = Just [YType.ConfigVersionMap {config = LYT.RIDER_CONFIG (LYT.UiConfig os platform), version = version}]
-- TDL.findOneUiConfig
--   (cast merchantOperatingCityId)
--   (LYT.RIDER_CONFIG (LYT.UiConfig os platform))
--   mbConfigInExperimentVersions
--   Nothing
--   (Queries.getUiConfig YType.UiConfigRequest {..} merchantOperatingCityId)
--   isBaseLogic

cacheAllUIConfigByMerchantOperatingCityDeviceTypePlatformType :: (CacheFlow m r) => Id MerchantOperatingCity -> DeviceType -> YType.PlatformType -> Maybe UiRiderConfig -> m ()
cacheAllUIConfigByMerchantOperatingCityDeviceTypePlatformType merchantOpCityId os plt config = do
  let key = makeRiderUiConfigKey merchantOpCityId os plt
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key config expTime

makeRiderUiConfigKey :: Id MerchantOperatingCity -> DeviceType -> YType.PlatformType -> Text
makeRiderUiConfigKey mocid os plt = "CachedQueries:UiRiderConfig:moc:" <> getId mocid <> ":os:" <> (Text.pack . show) os <> ":lang:" <> (Text.pack . show) plt

--------- Queries Reuqiring No Caching --------------------
create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => UiRiderConfig -> m ()
create = Queries.create

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => UiRiderConfig -> m ()
updateByPrimaryKey = Queries.updateByPrimaryKey

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> DeviceType -> YType.PlatformType -> m ()
clearCache _ _ _ = do
  throwError $ InvalidRequest $ "UI config not supported yet"

-- TDL.clearConfigCache
--   (cast mocid)
--   (LYT.RIDER_CONFIG (LYT.UiConfig dt pt))
--   Nothing
