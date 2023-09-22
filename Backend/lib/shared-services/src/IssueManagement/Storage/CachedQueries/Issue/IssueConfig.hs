{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module IssueManagement.Storage.CachedQueries.Issue.IssueConfig where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueConfig
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueConfig as Queries
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow)

findIssueConfig :: (CacheFlow m r, BeamFlow m) => Identifier -> m (Maybe IssueConfig)
findIssueConfig identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueConfigKey identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueConfig identifier /=<< Queries.findOne

--------- Caching logic -------------------

clearIssueConfigCache :: CacheFlow m r => Identifier -> m ()
clearIssueConfigCache identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueConfigKey identifier

cacheAllIssueConfig :: CacheFlow m r => Identifier -> Maybe IssueConfig -> m ()
cacheAllIssueConfig identifier issueConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueConfigKey identifier) issueConfig expTime

makeIssueConfigKey :: Identifier -> Text
makeIssueConfigKey identifier = show identifier <> "CachedQueries:IssueConfig"
