{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IssueManagement.Storage.CachedQueries.Issue.IssueConfig where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueConfig
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueConfig as Queries
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

create :: BeamFlow m r => IssueConfig -> m ()
create = Queries.create

findByMerchantOpCityId :: BeamFlow m r => Id MerchantOperatingCity -> Identifier -> m (Maybe IssueConfig)
findByMerchantOpCityId merchantOpCityId identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueConfigKeyByMerchantOpCityId merchantOpCityId identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueConfig merchantOpCityId identifier /=<< Queries.findByMerchantOpCityId merchantOpCityId

--------- Caching logic -------------------

clearIssueConfigCache :: CacheFlow m r => Id MerchantOperatingCity -> Identifier -> m ()
clearIssueConfigCache merchantOpCityId identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueConfigKeyByMerchantOpCityId merchantOpCityId identifier

updateByPrimaryKey :: BeamFlow m r => IssueConfig -> m ()
updateByPrimaryKey = Queries.updateByPrimaryKey

cacheAllIssueConfig :: CacheFlow m r => Id MerchantOperatingCity -> Identifier -> Maybe IssueConfig -> m ()
cacheAllIssueConfig merchantOpCityId identifier issueConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueConfigKeyByMerchantOpCityId merchantOpCityId identifier) issueConfig expTime

makeIssueConfigKeyByMerchantOpCityId :: Id MerchantOperatingCity -> Identifier -> Text
makeIssueConfigKeyByMerchantOpCityId merchantOpCityId identifier = show identifier <> ":CachedQueries:IssueConfig:MerchantOperatingCityId-" <> merchantOpCityId.getId
