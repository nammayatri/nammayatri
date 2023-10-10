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

import qualified Data.Time as T
import IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Beam.MediaFile as BeamMF
import IssueManagement.Storage.BeamFlow (BeamFlow)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Sequelize as Se

create :: BeamFlow m => DMF.MediaFile -> m ()
create = createWithKV

findById :: BeamFlow m => Id MediaFile -> m (Maybe MediaFile)
findById (Id mediaFileId) = findOneWithKV [Se.Is BeamMF.id $ Se.Eq mediaFileId]

findAllIn :: BeamFlow m => [Id MediaFile] -> m [MediaFile]
findAllIn mfList = findAllWithKV [Se.Is BeamMF.id $ Se.In $ getId <$> mfList]

deleteById :: BeamFlow m => Id MediaFile -> m ()
deleteById (Id mediaFileId) = deleteWithKV [Se.Is BeamMF.id (Se.Eq mediaFileId)]

instance FromTType' BeamMF.MediaFile MediaFile where
  fromTType' BeamMF.MediaFileT {..} = do
    pure $
      Just
        MediaFile
          { id = Id id,
            _type = fileType,
            url = url,
            createdAt = T.localTimeToUTC T.utc createdAt
          }

instance ToTType' BeamMF.MediaFile MediaFile where
  toTType' MediaFile {..} = do
    BeamMF.MediaFileT
      { BeamMF.id = getId id,
        BeamMF.fileType = _type,
        BeamMF.url = url,
        BeamMF.createdAt = T.utcToLocalTime T.utc createdAt
      }
