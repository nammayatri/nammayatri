{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.MediaFile where

import Domain.Types.MediaFile as DMF
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.MediaFile as BeamMF
import qualified Storage.Tabular.VechileNew as VN

-- create :: MediaFile -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DMF.MediaFile -> m (MeshResult ())
create mediaFile = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainMediaFileToBeam mediaFile)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById :: Transactionable m => Id MediaFile -> m (Maybe MediaFile)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id MediaFile -> m (Maybe MediaFile)
findById (Id mediaFileId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamMediaFileToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamMF.id $ Se.Eq mediaFileId]
    Nothing -> pure Nothing

-- findAllIn :: Transactionable m => [Id MediaFile] -> m [MediaFile]
-- findAllIn mfList =
--   Esq.findAll $ do
--     mediaFile <- from $ table @MediaFileT
--     where_ $ mediaFile ^. MediaFileId `in_` valList (map getId mfList)
--     return mediaFile

findAllIn :: L.MonadFlow m => [Id MediaFile] -> m [MediaFile]
findAllIn mfList = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamMediaFileToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamMF.id $ Se.In $ getId <$> mfList]
    Nothing -> pure []

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
  BeamMF.MediaFileT
    { BeamMF.id = getId id,
      BeamMF.fileType = _type,
      BeamMF.url = url,
      BeamMF.createdAt = createdAt
    }
