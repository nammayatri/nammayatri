{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.MerchantServiceUsageConfig where

import qualified Client.Main as CM
import Data.Aeson as A
import qualified Domain.Types.Cac as DTC
import Domain.Types.MerchantOperatingCity
import Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions as KBF
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Cac
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.MerchantServiceUsageConfig as SBMSUC
import Storage.Beam.SystemConfigs ()
import Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMMSUC
import Storage.Queries.MerchantServiceUsageConfig ()
import qualified Utils.Common.CacUtils as CCU

getConfigFromMemory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe MerchantServiceUsageConfig)
getConfigFromMemory id = do
  isExp <- DTC.updateConfig DTC.LastUpdatedMerchantServiceUsageConfig
  getConfigFromMemoryCommon (DTC.MerchantServiceUsageConfig id.getId) isExp CM.isExperimentsRunning

setConfigInMemory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe MerchantServiceUsageConfig -> m (Maybe MerchantServiceUsageConfig)
setConfigInMemory id config = do
  isExp <- DTC.inMemConfigUpdateTime DTC.LastUpdatedMerchantServiceUsageConfig
  CCU.setConfigInMemoryCommon (DTC.MerchantServiceUsageConfig id.getId) isExp config

findByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe CCU.CacKey -> m (Maybe MerchantServiceUsageConfig)
findByMerchantOpCityId id mbstickId = do
  inMemConfig <- getConfigFromMemory id
  let context = [(CCU.MerchantOperatingCityId, toJSON id.getId)]
  config <-
    CCU.getConfigFromCacOrDB inMemConfig context mbstickId (KBF.fromCacType @SBMSUC.MerchantServiceUsageConfig) CCU.MerchantServiceUsageConfig
      |<|>| ( do
                logDebug $ "MerchantServiceUsageConfig not found in memory, fetching from DB for context: " <> show context
                CMMSUC.findByMerchantOpCityId id
            )
  setConfigInMemory id config

--------------------------------------------------------- Things can't be handled by cac --------------------------------------------------

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceUsageConfig -> m ()
create = CMMSUC.create

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = CMMSUC.clearCache

updateMerchantServiceUsageConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceUsageConfig -> m ()
updateMerchantServiceUsageConfig = CMMSUC.updateMerchantServiceUsageConfig

instance KBF.FromCacType SBMSUC.MerchantServiceUsageConfig MerchantServiceUsageConfig where
  fromCacType = fromTType'
