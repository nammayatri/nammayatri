{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MediaFile where

import qualified Data.Time as T
import Domain.Types.MediaFile as DMF
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findAllWithKvInReplica, findOneWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.MediaFile as BeamMF

create :: (L.MonadFlow m, Log m) => DMF.MediaFile -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id MediaFile -> m (Maybe MediaFile)
findById (Id mediaFileId) = findOneWithKV [Se.Is BeamMF.id $ Se.Eq mediaFileId]

findAllIn :: (L.MonadFlow m, Log m) => [Id MediaFile] -> m [MediaFile]
findAllIn mfList = findAllWithKV [Se.Is BeamMF.id $ Se.In $ getId <$> mfList]

findAllInInReplica :: (L.MonadFlow m, Log m) => [Id MediaFile] -> m [MediaFile]
findAllInInReplica mfList = findAllWithKvInReplica [Se.Is BeamMF.id $ Se.In $ getId <$> mfList]

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

deleteById :: Id MediaFile -> SqlDB ()
deleteById = Esq.deleteByKey @MediaFileT

deleteById :: Id MediaFile -> SqlDB ()
deleteById = Esq.deleteByKey @MediaFileT
