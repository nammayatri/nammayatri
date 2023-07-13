{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RegistrationToken where

import Domain.Types.Person
import Domain.Types.RegistrationToken as DRT
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT

create :: L.MonadFlow m => DRT.RegistrationToken -> m (MeshResult ())
create registrationToken = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainRegistrationTokenToBeam registrationToken)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById (Id registrationTokenId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRT.id $ Se.Eq registrationTokenId]
    Nothing -> pure Nothing

setVerified :: (L.MonadFlow m, MonadTime m) => Id RegistrationToken -> m (MeshResult ())
setVerified (Id rtId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamRT.verified True,
          Se.Set BeamRT.updatedAt now
        ]
        [Se.Is BeamRT.id (Se.Eq rtId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findByToken :: L.MonadFlow m => RegToken -> m (Maybe RegistrationToken)
findByToken token = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRT.token $ Se.Eq token]
    Nothing -> pure Nothing

updateAttempts :: (L.MonadFlow m, MonadTime m) => Int -> Id RegistrationToken -> m (MeshResult ())
updateAttempts attempts (Id rtId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamRT.attempts attempts,
          Se.Set BeamRT.updatedAt now
        ]
        [Se.Is BeamRT.id (Se.Eq rtId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

deleteByPersonId :: L.MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamRT.entityId (Se.Eq personId)]
    Nothing -> pure ()

deleteByPersonIdExceptNew :: L.MonadFlow m => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]
    Nothing -> pure ()

findAllByPersonId :: L.MonadFlow m => Id Person -> m [RegistrationToken]
findAllByPersonId personId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRT.entityId $ Se.Eq $ getId personId]
    Nothing -> pure []

getAlternateNumberAttempts :: L.MonadFlow m => Id Person -> m Int
getAlternateNumberAttempts (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      rt <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamRT.entityId $ Se.Eq personId]
      case rt of
        Right (Just x) -> do
          let rt' = transformBeamRegistrationTokenToDomain x
          let attempts = DRT.attempts rt'
          pure attempts
        _ -> pure 0
    Nothing -> pure 0

transformBeamRegistrationTokenToDomain :: BeamRT.RegistrationToken -> RegistrationToken
transformBeamRegistrationTokenToDomain BeamRT.RegistrationTokenT {..} = do
  RegistrationToken
    { id = Id id,
      token = token,
      attempts = attempts,
      authMedium = authMedium,
      authType = authType,
      authValueHash = authValueHash,
      verified = verified,
      authExpiry = authExpiry,
      tokenExpiry = tokenExpiry,
      entityId = entityId,
      merchantId = merchantId,
      entityType = entityType,
      createdAt = createdAt,
      updatedAt = updatedAt,
      info = info,
      alternateNumberAttempts = alternateNumberAttempts
    }

transformDomainRegistrationTokenToBeam :: RegistrationToken -> BeamRT.RegistrationToken
transformDomainRegistrationTokenToBeam RegistrationToken {..} =
  BeamRT.RegistrationTokenT
    { BeamRT.id = getId id,
      BeamRT.token = token,
      BeamRT.attempts = attempts,
      BeamRT.authMedium = authMedium,
      BeamRT.authType = authType,
      BeamRT.authValueHash = authValueHash,
      BeamRT.verified = verified,
      BeamRT.authExpiry = authExpiry,
      BeamRT.tokenExpiry = tokenExpiry,
      BeamRT.entityId = entityId,
      BeamRT.merchantId = merchantId,
      BeamRT.entityType = entityType,
      BeamRT.createdAt = createdAt,
      BeamRT.updatedAt = updatedAt,
      BeamRT.info = info,
      BeamRT.alternateNumberAttempts = alternateNumberAttempts
    }
