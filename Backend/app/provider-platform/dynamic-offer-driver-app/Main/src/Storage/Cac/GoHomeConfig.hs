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

module Storage.Cac.GoHomeConfig where

import qualified Client.Main as CM
import Data.Aeson as DA
import qualified Domain.Types.Cac as DTC
import Domain.Types.GoHomeConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Beam.Functions as KBF
import Kernel.Prelude
import Kernel.Types.Cac
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import qualified Storage.Beam.GoHomeConfig as BeamGHC
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.GoHomeConfig as GHC
import qualified Storage.Queries.GoHomeConfig as Queries
import qualified Utils.Common.CacUtils as CCU

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => GoHomeConfig -> m ()
create = Queries.create

getConfigsFromMemory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe GoHomeConfig)
getConfigsFromMemory id = do
  isExpired <- DTC.updateConfig DTC.LastUpdatedGoHomeConfig
  getConfigFromMemoryCommon (DTC.GoHomeConfig id.getId) isExpired CM.isExperimentsRunning

setConfigInMemory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe GoHomeConfig -> m (Maybe GoHomeConfig)
setConfigInMemory id config = do
  isExp <- DTC.inMemConfigUpdateTime DTC.LastUpdatedGoHomeConfig
  CCU.setConfigInMemoryCommon (DTC.GoHomeConfig id.getId) isExp config

findByMerchantOpCityId :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe CCU.CacKey -> m GoHomeConfig
findByMerchantOpCityId id stickyId = do
  let context = [(CCU.MerchantOperatingCityId, DA.toJSON id.getId)]
  inMemConfig <- getConfigsFromMemory id
  config' <-
    CCU.getConfigFromCacOrDB inMemConfig context stickyId (fromCacType @BeamGHC.GoHomeConfig) CCU.GoHomeConfig
      |<|>| ( do
                logDebug $ "GoHomeConfig not found in memory, fetching from DB for context: " <> show context
                GHC.getGoHomeConfigFromDB id
            )
  config <- setConfigInMemory id config'
  case config of
    Nothing -> throwError $ InvalidRequest $ "GoHome Config not found for MerchantOperatingCity: " <> id.getId
    Just cfg -> pure cfg

instance FromCacType BeamGHC.GoHomeConfig GoHomeConfig where
  fromCacType = fromTType'
