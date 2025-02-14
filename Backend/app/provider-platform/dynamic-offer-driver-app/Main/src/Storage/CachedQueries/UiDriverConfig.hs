{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.UiDriverConfig where

import Data.Aeson as A
import qualified Data.Text as Text
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.UiDriverConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version (DeviceType)
import Kernel.Utils.Common (CacheFlow, fromMaybeM)
import Kernel.Utils.Logging
import Kernel.Utils.Time
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as YType
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Queries.UiDriverConfig as Queries
import qualified Tools.DynamicLogic as TDL

findUIConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => YType.UiConfigRequest -> Id MerchantOperatingCity -> m (Maybe UiDriverConfig, Maybe Int)
findUIConfig YType.UiConfigRequest {..} merchantOperatingCityId = do
  let key = makeDriverUiConfigKey merchantOperatingCityId os platform
  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc -- bounds, all these params, timeDiffFromUTC
  config' <-
    Hedis.safeGet key >>= \case
      Just config -> pure config
      Nothing -> do
        cacheAllTollsByMerchantOperatingCity merchantOperatingCityId os platform /=<< Queries.findUIConfig YType.UiConfigRequest {..} merchantOperatingCityId
  case config' of
    Just config -> do
      (allLogics, version) <- TDL.getAppDynamicLogic (cast merchantOperatingCityId) (LYT.UI_DRIVER os platform) localTime Nothing toss
      resp <- LYTU.runLogics allLogics config
      case (fromJSON resp.result :: Result UiDriverConfig) of
        Success dpc'' -> pure (Just dpc'', version)
        A.Error e -> do
          logError $ "Error in applying dynamic logic: " <> show e
          incrementSystemConfigsFailedCounter "driver_ui_config_dynamic_logic_failure"
          pure (Just config, version)
    Nothing -> pure (Nothing, Nothing)

cacheAllTollsByMerchantOperatingCity :: (CacheFlow m r) => Id MerchantOperatingCity -> DeviceType -> YType.PlatformType -> Maybe UiDriverConfig -> m ()
cacheAllTollsByMerchantOperatingCity merchantOpCityId os plt config = do
  let key = makeDriverUiConfigKey merchantOpCityId os plt
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key config expTime

makeDriverUiConfigKey :: Id MerchantOperatingCity -> DeviceType -> YType.PlatformType -> Text
makeDriverUiConfigKey mocid os plt = "CachedQueries:UiDriverConfig:moc:" <> getId mocid <> ":os:" <> (Text.pack . show) os <> ":lang:" <> (Text.pack . show) plt

--------- Queries Reuqiring No Caching --------------------
create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => UiDriverConfig -> m ()
create = Queries.create

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => UiDriverConfig -> m ()
updateByPrimaryKey = Queries.updateByPrimaryKey
