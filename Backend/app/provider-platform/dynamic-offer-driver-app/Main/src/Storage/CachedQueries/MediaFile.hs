{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.MediaFile where

import Domain.Types.Issue.IssueReport (IssueReport)
import Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Storage.Queries.MediaFile as Queries

findById :: (CacheFlow m r, MonadFlow m) => Id MediaFile -> m (Maybe MediaFile)
findById mediaFileId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMediaFileByIdKey mediaFileId) >>= \case
    Just a -> pure a
    Nothing -> cacheMediaFileById mediaFileId /=<< Queries.findById mediaFileId

findAllInForIssueReportId :: (CacheFlow m r, Esq.EsqDBFlow m r) => [Id MediaFile] -> Id IssueReport -> m [MediaFile]
findAllInForIssueReportId mediaFileIds issueReportId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMediaFileByIssueReportIdKey issueReportId) >>= \case
    Just a -> pure a
    Nothing -> cacheMediaFileByIssueReportId issueReportId /=<< Queries.findAllIn mediaFileIds

--------- Caching logic for media file by id -------------------

clearMediaFileByIdCache :: (CacheFlow m r) => Id MediaFile -> m ()
clearMediaFileByIdCache = Hedis.withCrossAppRedis . Hedis.del . makeMediaFileByIdKey

cacheMediaFileById :: (CacheFlow m r) => Id MediaFile -> Maybe MediaFile -> m ()
cacheMediaFileById mediaFileId mediaFile = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMediaFileByIdKey mediaFileId) mediaFile expTime

makeMediaFileByIdKey :: Id MediaFile -> Text
makeMediaFileByIdKey id = "driver-offer:CachedQueries:MediaFile:Id-" <> show id

--------- Caching logic for media files by issue report id -------------------

clearMediaFileByIssueReportIdCache :: (CacheFlow m r) => Id IssueReport -> m ()
clearMediaFileByIssueReportIdCache = Hedis.withCrossAppRedis . Hedis.del . makeMediaFileByIssueReportIdKey

cacheMediaFileByIssueReportId :: (CacheFlow m r) => Id IssueReport -> [MediaFile] -> m ()
cacheMediaFileByIssueReportId issueReportId mediaFiles = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMediaFileByIssueReportIdKey issueReportId) mediaFiles expTime

makeMediaFileByIssueReportIdKey :: Id IssueReport -> Text
makeMediaFileByIssueReportIdKey issueReportId = "driver-offer:CachedQueries:MediaFile:IssueReport:Id-" <> show issueReportId

invalidateMediaFileCache :: (CacheFlow m r) => [Id MediaFile] -> Maybe (Id IssueReport) -> m ()
invalidateMediaFileCache mediaFileIds mbIssueReportId = do
  mapM_ clearMediaFileByIdCache mediaFileIds
  whenJust mbIssueReportId clearMediaFileByIssueReportIdCache
