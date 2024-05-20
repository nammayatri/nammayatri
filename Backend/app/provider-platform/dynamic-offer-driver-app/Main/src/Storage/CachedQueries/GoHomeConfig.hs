{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.GoHomeConfig
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ But most likely you need a version from Cac with inMem results feature."
    #-}
where

import qualified Client.Main as CM
import Control.Monad
import Data.Text as Text
import qualified Domain.Types.Cac as DTC
import Domain.Types.GoHomeConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Cac
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.GoHomeConfig as Queries

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => GoHomeConfig -> m ()
create = Queries.create

getGoHomeConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe GoHomeConfig)
getGoHomeConfigFromDB id = do
  Hedis.safeGet (makeGoHomeKey id) >>= \case
    Just cfg -> return cfg
    Nothing -> do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      cfg <- fromMaybeM (InternalError ("Could not find Go-To config corresponding to the stated merchant id" <> show id)) =<< Queries.findByMerchantOpCityId id
      Hedis.setExp (makeGoHomeKey id) cfg expTime
      return (Just cfg)

getConfigsFromMemory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe GoHomeConfig)
getConfigsFromMemory id = do
  isExpired <- DTC.updateConfig DTC.LastUpdatedGoHomeConfig
  getConfigFromMemoryCommon (DTC.GoHomeConfig id.getId) isExpired CM.isExperimentsRunning

findByMerchantOpCityId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> m GoHomeConfig
findByMerchantOpCityId id = getGoHomeConfigFromDB id <&> fromJust

makeGoHomeKey :: Id MerchantOperatingCity -> Text
makeGoHomeKey id = "driver-offer:CachedQueries:GoHomeConfig:MerchantOpCityId-" <> id.getId
