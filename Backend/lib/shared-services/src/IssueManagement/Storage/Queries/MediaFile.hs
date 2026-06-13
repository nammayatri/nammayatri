{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.MediaFile where

import qualified Data.Text as DT
import qualified Data.Time as T
import IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Beam.MediaFile as BeamMF
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => DMF.MediaFile -> m ()
create = createWithKV

findById :: BeamFlow m r => Id MediaFile -> m (Maybe MediaFile)
findById (Id mediaFileId) = findOneWithKV [Is BeamMF.id $ Eq mediaFileId]

findAllIn :: BeamFlow m r => [Id MediaFile] -> m [MediaFile]
findAllIn mfList = findAllWithKV [Is BeamMF.id $ In $ getId <$> mfList]

deleteById :: BeamFlow m r => Id MediaFile -> m ()
deleteById (Id mediaFileId) = deleteWithKV [Is BeamMF.id (Eq mediaFileId)]

updateStatusById :: BeamFlow m r => DMF.MediaFileUploadStatus -> Id MediaFile -> m ()
updateStatusById newStatus (Id mediaFileId) =
  updateWithKV
    [Set BeamMF.status (Just $ DT.pack $ show newStatus)]
    [Is BeamMF.id $ Eq mediaFileId]

updateStatusAndHashById :: BeamFlow m r => DMF.MediaFileUploadStatus -> Maybe Text -> Id MediaFile -> m ()
updateStatusAndHashById newStatus fileHash (Id mediaFileId) =
  updateWithKV
    [Set BeamMF.status (Just $ DT.pack $ show newStatus), Set BeamMF.fileHash fileHash]
    [Is BeamMF.id $ Eq mediaFileId]

instance FromTType' BeamMF.MediaFile MediaFile where
  fromTType' BeamMF.MediaFileT {..} = do
    pure $
      Just
        MediaFile
          { id = Id id,
            _type = fileType,
            url = url,
            s3FilePath = s3FilePath,
            status = status >>= readMaybe . DT.unpack,
            fileHash = fileHash,
            createdAt = T.localTimeToUTC T.utc createdAt
          }

instance ToTType' BeamMF.MediaFile MediaFile where
  toTType' MediaFile {..} = do
    BeamMF.MediaFileT
      { BeamMF.id = getId id,
        BeamMF.fileType = _type,
        BeamMF.url = url,
        BeamMF.s3FilePath = s3FilePath,
        BeamMF.status = DT.pack . show <$> status,
        BeamMF.fileHash = fileHash,
        BeamMF.createdAt = T.utcToLocalTime T.utc createdAt
      }
