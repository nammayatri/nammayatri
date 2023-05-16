{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.MediaFile where

import Domain.Types.MediaFile
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.MediaFile as BeamMF
import Storage.Tabular.MediaFile

create :: MediaFile -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id MediaFile -> m (Maybe MediaFile)
findById = Esq.findById

findAllIn :: Transactionable m => [Id MediaFile] -> m [MediaFile]
findAllIn mfList =
  Esq.findAll $ do
    mediaFile <- from $ table @MediaFileT
    where_ $ mediaFile ^. MediaFileId `in_` valList (map getId mfList)
    return mediaFile

transformBeamMediaFileToDomain :: BeamMF.MediaFile -> MediaFile
transformBeamMediaFileToDomain BeamMF.MediaFileT {..} = do
  MediaFile
    { id = Id id,
      _type = fileType,
      url = url,
      createdAt = createdAt
    }

transformDomainMediaFileToBeam :: MediaFile -> BeamMF.MediaFile
transformDomainMediaFileToBeam MediaFile {..} =
  BeamMF.defaultMediaFile
    { BeamMF.id = getId id,
      BeamMF.fileType = _type,
      BeamMF.url = url,
      BeamMF.createdAt = createdAt
    }
