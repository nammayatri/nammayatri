{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.LeaderBoardConfig where

import Domain.Types.LeaderBoardConfig
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.LeaderBoardConfig as Queries

findLeaderBoardConfigbyType :: (CacheFlow m r, Esq.EsqDBFlow m r) => LeaderBoardType -> Id Merchant -> m (Maybe LeaderBoardConfigs)
findLeaderBoardConfigbyType leaderBType merchantId =
  Hedis.safeGet (makeLeaderBoardConfigKey leaderBType merchantId) >>= \case
    Just config -> pure $ Just config
    Nothing -> flip whenJust (cacheLeaderBoardConfig leaderBType merchantId) /=<< Queries.findLeaderBoardConfigbyType leaderBType merchantId

makeLeaderBoardConfigKey :: LeaderBoardType -> Id Merchant -> Text
makeLeaderBoardConfigKey leaderBType merchantId = "LBCFG:" <> merchantId.getId <> ":" <> show leaderBType

cacheLeaderBoardConfig :: (CacheFlow m r) => LeaderBoardType -> Id Merchant -> LeaderBoardConfigs -> m ()
cacheLeaderBoardConfig leaderBType merchantId lbConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeLeaderBoardConfigKey leaderBType merchantId) lbConfig expTime
