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

module Storage.Cac.DriverIntelligentPoolConfig
  ( findByMerchantOpCityId,
    create,
    clearCache,
    update,
  )
where

import Client.Main as CM
import Data.Aeson as DA
import qualified Domain.Types.Cac as DTC
import Domain.Types.DriverIntelligentPoolConfig
import Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions as KBF
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Cac
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.DriverIntelligentPoolConfig as SBMDIPC
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig as CMDP
import Utils.Common.CacUtils as CCU

getConfigFromInMemory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe DriverIntelligentPoolConfig)
getConfigFromInMemory id = do
  isExp <- DTC.updateConfig DTC.LastUpdatedDriverPoolConfig
  getConfigFromMemoryCommon (DTC.DriverIntelligentPoolConfig id.getId) isExp CM.isExperimentsRunning

setConfigInMemory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe DriverIntelligentPoolConfig -> m (Maybe DriverIntelligentPoolConfig)
setConfigInMemory id config = do
  isExp <- DTC.inMemConfigUpdateTime DTC.LastUpdatedDriverIntelligentPoolConfig
  CCU.setConfigInMemoryCommon (DTC.DriverIntelligentPoolConfig id.getId) isExp config

findByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe CacKey -> m (Maybe DriverIntelligentPoolConfig)
findByMerchantOpCityId id stickeyKey = do
  let context = [(CCU.MerchantOperatingCityId, DA.toJSON id.getId)]
  inMemConfig <- getConfigFromInMemory id
  config <-
    CCU.getConfigFromCacOrDB inMemConfig context stickeyKey (KBF.fromCacType @SBMDIPC.DriverIntelligentPoolConfig) CCU.DriverIntelligentPoolConfig
      |<|>| ( do
                logDebug $ "DriverIntelligentPoolConfig not found in memory, fetching from DB for context: " <> show context
                CMDP.getDriverIntelligentPoolConfigFromDB id
            )
  setConfigInMemory id config

---------------------------------------------------------cached queries for dashboard---------------------------------------------------------
create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverIntelligentPoolConfig -> m ()
create = CMDP.create

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = CMDP.clearCache

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverIntelligentPoolConfig -> m ()
update = CMDP.update

instance FromCacType SBMDIPC.DriverIntelligentPoolConfig DriverIntelligentPoolConfig where
  fromCacType = fromTType'
