{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person.PersonDefaultEmergencyNumber where

import Domain.Types.Person
import Domain.Types.Person.PersonDefaultEmergencyNumber
-- import Kernel.Storage.Esqueleto as Esq

-- import Storage.Tabular.Person.PersonDefaultEmergencyNumber
-- import EulerHS.KVConnector.Types

import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Person.PersonDefaultEmergencyNumber as BeamPDEN

-- replaceAll :: Id Person -> [PersonDefaultEmergencyNumber] -> SqlDB ()
-- replaceAll personId pdenList = do
--   Esq.delete $ do
--     personENT <- from $ table @PersonDefaultEmergencyNumberT
--     where_ $ personENT ^. PersonDefaultEmergencyNumberTId ==. val (toKey personId)
--   Esq.createMany pdenList

create :: L.MonadFlow m => PersonDefaultEmergencyNumber -> m ()
create pden = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamPDEN.PersonDefaultEmergencyNumberT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainPersonDefaultEmergencyNumberToBeam pden)
    Nothing -> pure ()

createMany :: L.MonadFlow m => [PersonDefaultEmergencyNumber] -> m ()
createMany = traverse_ create

replaceALL :: L.MonadFlow m => Id Person -> [PersonDefaultEmergencyNumber] -> m ()
replaceALL (Id personId) pdenList = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamPDEN.PersonDefaultEmergencyNumberT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.deleteWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamPDEN.personId $ Se.Eq personId]
    Nothing -> pure ()
  createMany pdenList

-- findAllByPersonId ::
--   Transactionable m =>
--   Id Person ->
--   m [PersonDefaultEmergencyNumber]
-- findAllByPersonId personId =
--   Esq.findAll $ do
--     personENT <- from $ table @PersonDefaultEmergencyNumberT
--     where_ $
--       personENT ^. PersonDefaultEmergencyNumberTId ==. val (toKey personId)
--     return personENT

findAllByPersonId :: L.MonadFlow m => Id Person -> m [PersonDefaultEmergencyNumber]
findAllByPersonId (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamPDEN.PersonDefaultEmergencyNumberT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      pden <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamPDEN.personId $ Se.Eq personId]
      case pden of
        Right result -> pure $ transformBeamPersonDefaultEmergencyNumberToDomain <$> result
        Left _ -> pure []
    Nothing -> pure []

transformBeamPersonDefaultEmergencyNumberToDomain :: BeamPDEN.PersonDefaultEmergencyNumber -> PersonDefaultEmergencyNumber
transformBeamPersonDefaultEmergencyNumberToDomain BeamPDEN.PersonDefaultEmergencyNumberT {..} = do
  PersonDefaultEmergencyNumber
    { personId = Id personId,
      name = name,
      mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
      mobileCountryCode = mobileCountryCode,
      createdAt = createdAt
    }

transformDomainPersonDefaultEmergencyNumberToBeam :: PersonDefaultEmergencyNumber -> BeamPDEN.PersonDefaultEmergencyNumber
transformDomainPersonDefaultEmergencyNumberToBeam PersonDefaultEmergencyNumber {..} =
  BeamPDEN.defaultPersonDefaultEmergencyNumber
    { BeamPDEN.personId = getId personId,
      BeamPDEN.name = name,
      BeamPDEN.mobileCountryCode = mobileCountryCode,
      BeamPDEN.mobileNumberHash = mobileNumber.hash,
      BeamPDEN.mobileNumberEncrypted = unEncrypted (mobileNumber.encrypted),
      BeamPDEN.createdAt = createdAt
    }
