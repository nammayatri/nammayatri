{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.MediaFile where

import qualified Data.Time as T
import Domain.Types.MediaFile as DMF
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.MediaFile as BeamMF

create :: L.MonadFlow m => DMF.MediaFile -> m (MeshResult ())
create mediaFile = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamMF.MediaFileT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainMediaFileToBeam mediaFile)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id MediaFile -> m (Maybe MediaFile)
findById (Id mediaFileId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamMF.MediaFileT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamMediaFileToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamMF.id $ Se.Eq mediaFileId]
    Nothing -> pure Nothing

findAllIn :: L.MonadFlow m => [Id MediaFile] -> m [MediaFile]
findAllIn mfList = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamMF.MediaFileT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamMediaFileToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamMF.id $ Se.In $ getId <$> mfList]
    Nothing -> pure []

transformBeamMediaFileToDomain :: BeamMF.MediaFile -> MediaFile
transformBeamMediaFileToDomain BeamMF.MediaFileT {..} = do
  MediaFile
    { id = Id id,
      _type = fileType,
      url = url,
      createdAt = T.localTimeToUTC T.utc createdAt
    }

transformDomainMediaFileToBeam :: MediaFile -> BeamMF.MediaFile
transformDomainMediaFileToBeam MediaFile {..} =
  BeamMF.MediaFileT
    { BeamMF.id = getId id,
      BeamMF.fileType = _type,
      BeamMF.url = url,
      BeamMF.createdAt = T.utcToLocalTime T.utc createdAt
    }
