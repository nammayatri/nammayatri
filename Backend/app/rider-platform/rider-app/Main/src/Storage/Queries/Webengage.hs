{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Webengage where

import Domain.Types.Webengage
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Webengage as BeamW
import Storage.Tabular.Webengage

create :: L.MonadFlow m => Webengage -> m (MeshResult ())
create Webengage = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamW.WebEngageT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainWebengageToBeam Webengage)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: Transactionable m => Id Webengage -> m (Maybe Webengage)
findById = Esq.findById

findById' :: L.MonadFlow m => Id Webengage -> m (Maybe Webengage)
findById' webengageId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamW.WebengageT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamWebengageToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamW.id $ Se.Eq (getId webengageId)]
    Nothing -> pure Nothing

findByInfoMsgId :: Transactionable m => Text -> m (Maybe Webengage)
findByInfoMsgId infoMessageId =
  Esq.findOne $ do
    webengage <- from $ table @WebengageT
    where_ $ webengage ^. WebengageInfoMessageId ==. val infoMessageId
    return webengage

findByInfoMsgId' :: L.MonadFlow m => Text -> m (Maybe Webengage)
findByInfoMsgId' infoMessageId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamW.WebengageT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamWebengageToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamW.infoMessageId $ Se.Eq infoMessageId]
    Nothing -> pure Nothing

transformBeamWebengageToDomain :: BeamW.Webengage -> Webengage
transformBeamWebengageToDomain BeamW.WebengageT {..} = do
  Webengage
    { id = Id id,
      version = version,
      contentTemplateId = contentTemplateId,
      principalEntityId = principalEntityId,
      infoMessageId = infoMessageId,
      webMessageId = webMessageId,
      toNumber = toNumber,
      status = status
    }

transformDomainWebengageToBeam :: Webengage -> BeamW.Webengage
transformDomainWebengageToBeam Webengage {..} =
  BeamW.defaultWebengage
    { BeamW.id = getId id,
      BeamW.version = version,
      BeamW.contentTemplateId = contentTemplateId,
      BeamW.principalEntityId = principalEntityId,
      BeamW.infoMessageId = infoMessageId,
      BeamW.webMessageId = webMessageId,
      BeamW.toNumber = toNumber,
      BeamW.status = status
    }
