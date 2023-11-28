{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Merchant.LeaderBoardConfig where

import Domain.Types.Merchant.ConfigMapping (ConfigMapping)
import Domain.Types.Merchant.LeaderBoardConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common (Meters)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.CachedQueries.Merchant.ConfigMapping as CMQ
import qualified Storage.Queries.Merchant.LeaderBoardConfig as Queries

findLeaderBoardConfigbyType :: (CacheFlow m r, Esq.EsqDBFlow m r) => LeaderBoardType -> Id MerchantOperatingCity -> Meters -> Maybe Variant -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType merchantOpCityId distance mbvt = do
  currTime <- getLocalCurrentTime 19800
  cmId <- CMQ.getConfigMapId merchantOpCityId distance mbvt currTime "leader_board_config" >>= fromMaybeM (InternalError $ "ConfigMapping not found for LeaderBoardConfig : mocid, distance, mbvt, currTime" <> show id <> "," <> show distance <> ", " <> show mbvt <> ", " <> show currTime)
  Hedis.safeGet (makeConfigMapKey leaderBType cmId) >>= \case
    Just config -> pure $ Just config
    Nothing -> flip whenJust (cacheLeaderBoardConfig leaderBType cmId) /=<< Queries.findLeaderBoardConfigbyTypeAndConfigMapping leaderBType cmId

makeLeaderBoardConfigKey :: LeaderBoardType -> Id MerchantOperatingCity -> Text
makeLeaderBoardConfigKey leaderBType merchantOpCityId = "LBCFG:" <> merchantOpCityId.getId <> ":" <> show leaderBType

makeConfigMapKey :: LeaderBoardType -> Id ConfigMapping -> Text
makeConfigMapKey lbt id = "driver-offer:CachedQueries:ConfigMapping:ConfigMapId-" <> id.getId <> ":LeaderBoardType-" <> show lbt

cacheLeaderBoardConfig :: (CacheFlow m r) => LeaderBoardType -> Id ConfigMapping -> LeaderBoardConfigs -> m ()
cacheLeaderBoardConfig leaderBType cmId lbConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeConfigMapKey leaderBType cmId) lbConfig expTime
