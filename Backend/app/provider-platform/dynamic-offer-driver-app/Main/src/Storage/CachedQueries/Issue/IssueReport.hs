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

module Storage.CachedQueries.Issue.IssueReport where

import Domain.Types.Issue.IssueReport
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Issue.IssueReport as Queries

findAllByDriver :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id SP.Person -> m [IssueReport]
findAllByDriver driverId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueReportByDriverKey driverId) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueReportByDriver driverId /=<< Queries.findAllByDriver driverId

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueReport -> m (Maybe IssueReport)
findById issueReportId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueReportByIdKey issueReportId) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueReportById issueReportId /=<< Queries.findById issueReportId

--------- Caching logic for issue Report by DriverId -------------------

clearIssueReportByDriverCache :: (CacheFlow m r) => Id SP.Person -> m ()
clearIssueReportByDriverCache = Hedis.withCrossAppRedis . Hedis.del . makeIssueReportByDriverKey

cacheAllIssueReportByDriver :: (CacheFlow m r) => Id SP.Person -> [IssueReport] -> m ()
cacheAllIssueReportByDriver driverId issueReport = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueReportByDriverKey driverId) issueReport expTime

makeIssueReportByDriverKey :: Id SP.Person -> Text
makeIssueReportByDriverKey driverId = "driver-offer:CachedQueries:IssueReport:Driver:Id-" <> show driverId

--------- Caching logic for issue Report by id -------------------

clearIssueReportByIdCache :: (CacheFlow m r) => Id IssueReport -> m ()
clearIssueReportByIdCache = Hedis.withCrossAppRedis . Hedis.del . makeIssueReportByIdKey

cacheIssueReportById :: (CacheFlow m r) => Id IssueReport -> Maybe IssueReport -> m ()
cacheIssueReportById issueReportId issueReport = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueReportByIdKey issueReportId) issueReport expTime

makeIssueReportByIdKey :: Id IssueReport -> Text
makeIssueReportByIdKey id = "driver-offer:CachedQueries:IssueReport:Id-" <> show id

invalidateIssueReportCache :: (CacheFlow m r) => Maybe (Id IssueReport) -> Maybe (Id SP.Person) -> m ()
invalidateIssueReportCache issueReportId driverId = do
  whenJust issueReportId clearIssueReportByIdCache
  whenJust driverId clearIssueReportByDriverCache
