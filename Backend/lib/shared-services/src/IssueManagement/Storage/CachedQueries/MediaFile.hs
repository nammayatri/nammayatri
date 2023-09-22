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

module IssueManagement.Storage.CachedQueries.MediaFile where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueReport (IssueReport)
import IssueManagement.Domain.Types.MediaFile
import IssueManagement.Storage.CachedQueries.CacheConfig
import qualified IssueManagement.Storage.Queries.MediaFile as Queries
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id

findById :: (CacheFlow m r, MonadFlow m) => Id MediaFile -> Identifier -> m (Maybe MediaFile)
findById mediaFileId identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMediaFileByIdKey mediaFileId identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheMediaFileById mediaFileId identifier /=<< Queries.findById mediaFileId

findAllInForIssueReportId :: (CacheFlow m r, Esq.EsqDBFlow m r) => [Id MediaFile] -> Id IssueReport -> Identifier -> m [MediaFile]
findAllInForIssueReportId mediaFileIds issueReportId identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMediaFileByIssueReportIdKey issueReportId identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheMediaFileByIssueReportId issueReportId identifier /=<< Queries.findAllIn mediaFileIds

--------- Caching logic for media file by id -------------------

clearMediaFileByIdCache :: (CacheFlow m r) => Identifier -> Id MediaFile -> m ()
clearMediaFileByIdCache identifier mediaFileId = Hedis.withCrossAppRedis . Hedis.del $ makeMediaFileByIdKey mediaFileId identifier

cacheMediaFileById :: (CacheFlow m r) => Id MediaFile -> Identifier -> Maybe MediaFile -> m ()
cacheMediaFileById mediaFileId identifier mediaFile = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMediaFileByIdKey mediaFileId identifier) mediaFile expTime

makeMediaFileByIdKey :: Id MediaFile -> Identifier -> Text
makeMediaFileByIdKey id identifier = show identifier <> "CachedQueries:MediaFile:Id-" <> show id

--------- Caching logic for media files by issue report id -------------------

clearMediaFileByIssueReportIdCache :: (CacheFlow m r) => Identifier -> Id IssueReport -> m ()
clearMediaFileByIssueReportIdCache identifier issueReportId = Hedis.withCrossAppRedis . Hedis.del $ makeMediaFileByIssueReportIdKey issueReportId identifier

cacheMediaFileByIssueReportId :: (CacheFlow m r) => Id IssueReport -> Identifier -> [MediaFile] -> m ()
cacheMediaFileByIssueReportId issueReportId identifier mediaFiles = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMediaFileByIssueReportIdKey issueReportId identifier) mediaFiles expTime

makeMediaFileByIssueReportIdKey :: Id IssueReport -> Identifier -> Text
makeMediaFileByIssueReportIdKey issueReportId identifier = show identifier <> "CachedQueries:MediaFile:IssueReport:Id-" <> show issueReportId

invalidateMediaFileCache :: (CacheFlow m r) => [Id MediaFile] -> Maybe (Id IssueReport) -> Identifier -> m ()
invalidateMediaFileCache mediaFileIds mbIssueReportId identifier = do
  mapM_ (clearMediaFileByIdCache identifier) mediaFileIds
  whenJust mbIssueReportId $ clearMediaFileByIssueReportIdCache identifier
