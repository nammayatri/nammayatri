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

module Storage.Cac.TransporterConfig where

import qualified Client.Main as CM
import Data.Aeson as A
import qualified Domain.Types.Cac as DTC
import Domain.Types.MerchantOperatingCity
import Domain.Types.TransporterConfig
import Kernel.Beam.Functions as KBF
import Kernel.Prelude as KP
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Cac
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Beam.TransporterConfig as SBMT
import Storage.CachedQueries.Merchant.TransporterConfig as CMTC
import Storage.Queries.TransporterConfig ()
import qualified Utils.Common.CacUtils as CCU

getConfigFromMemory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe TransporterConfig)
getConfigFromMemory id = do
  isExp <- DTC.updateConfig DTC.LastUpdatedTransporterConfig
  getConfigFromMemoryCommon (DTC.TransporterConfig id.getId) isExp CM.isExperimentsRunning

setConfigInMemory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe TransporterConfig -> m (Maybe TransporterConfig)
setConfigInMemory id config = do
  isExp <- DTC.inMemConfigUpdateTime DTC.LastUpdatedTransporterConfig
  CCU.setConfigInMemoryCommon (DTC.TransporterConfig id.getId) isExp config

findByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe CCU.CacKey -> m (Maybe TransporterConfig)
findByMerchantOpCityId id mbstickId = do
  inMemConfig <- getConfigFromMemory id
  let context = [(CCU.MerchantOperatingCityId, toJSON id.getId)]
  config <-
    CCU.getConfigFromCacOrDB inMemConfig context mbstickId (KBF.fromCacType @SBMT.TransporterConfig) CCU.TransporterConfig
      |<|>| ( do
                logDebug $ "TransporterConfig not found in memory, fetching from DB for context: " <> show context
                CMTC.getTransporterConfigFromDB id
            )
  setConfigInMemory id config

--------------------------------------------------------- Things can't be handled by cac --------------------------------------------------

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
create = CMTC.create

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = CMTC.clearCache

updateFCMConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> BaseUrl -> Text -> m ()
updateFCMConfig = CMTC.updateFCMConfig

updateReferralLinkPassword :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> m ()
updateReferralLinkPassword = CMTC.updateReferralLinkPassword

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
update = CMTC.update

instance KBF.FromCacType SBMT.TransporterConfig TransporterConfig where
  fromCacType = fromTType'
