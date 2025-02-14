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

import Data.Aeson as A
import qualified Data.Text as Text
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.UiRiderConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version (DeviceType)
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Logging
import Kernel.Utils.Time
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as YType
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.UiRiderConfig as Queries
import qualified Tools.DynamicLogic as TDL

findUiConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => YType.UiConfigRequest -> Id MerchantOperatingCity -> m (Maybe UiRiderConfig, Maybe Int)
findUiConfig YType.UiConfigRequest {..} merchantOperatingCityId = do
  let key = makeRiderUiConfigKey merchantOperatingCityId os platform
  localTime <- getLocalCurrentTime 19800 -- Fix Me
  config' <-
    Hedis.safeGet key >>= \case
      Just config -> pure config
      Nothing -> do
        cacheAllTollsByMerchantOperatingCity merchantOperatingCityId os platform /=<< Queries.getUiConfig YType.UiConfigRequest {..} merchantOperatingCityId
  case config' of
    Just config -> do
      (allLogics, version) <- TDL.getAppDynamicLogic (cast merchantOperatingCityId) (LYT.UI_RIDER os platform) localTime Nothing toss
      resp <- LYTU.runLogics allLogics config
      case (fromJSON resp.result :: Result UiRiderConfig) of
        Success dpc'' -> pure (Just dpc'', version)
        A.Error e -> do
          logError $ "Error in applying dynamic logic: " <> show e
          incrementSystemConfigsFailedCounter "Rider_ui_config_dynamic_logic_failure"
          pure (Just config, version)
    Nothing -> pure (Nothing, Nothing)

cacheAllTollsByMerchantOperatingCity :: (CacheFlow m r) => Id MerchantOperatingCity -> DeviceType -> YType.PlatformType -> Maybe UiRiderConfig -> m ()
cacheAllTollsByMerchantOperatingCity merchantOpCityId os plt config = do
  let key = makeRiderUiConfigKey merchantOpCityId os plt
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key config expTime

makeRiderUiConfigKey :: Id MerchantOperatingCity -> DeviceType -> YType.PlatformType -> Text
makeRiderUiConfigKey mocid os plt = "CachedQueries:UiRiderConfig:moc:" <> getId mocid <> ":os:" <> (Text.pack . show) os <> ":lang:" <> (Text.pack . show) plt

--------- Queries Reuqiring No Caching --------------------
create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => UiRiderConfig -> m ()
create = Queries.create
