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
import Domain.Types.RegistrationToken
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as BeamRT

create :: L.MonadFlow m => RegistrationToken -> m (MeshResult ())
create registrationToken = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainRegistrationTokenToBeam registrationToken)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById :: Transactionable m => Id RegistrationToken -> m (Maybe RegistrationToken)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById (Id registrationTokenId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRT.id $ Se.Eq registrationTokenId]
    Nothing -> pure Nothing

-- findByToken :: Transactionable m => Text -> m (Maybe RegistrationToken)
-- findByToken token_ =
--   findOne $ do
--     registrationToken <- from $ table @RegistrationTokenT
--     where_ $ registrationToken ^. RegistrationTokenToken ==. val token_
--     return registrationToken

findByToken :: L.MonadFlow m => RegToken -> m (Maybe RegistrationToken)
findByToken token = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRT.token $ Se.Eq token]
    Nothing -> pure Nothing

-- setVerified :: Id RegistrationToken -> SqlDB ()
-- setVerified rtId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RegistrationTokenUpdatedAt =. val now,
--         RegistrationTokenVerified =. val True
--       ]
--     where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

setVerified :: (L.MonadFlow m, MonadTime m) => Id RegistrationToken -> m (MeshResult ())
setVerified (Id rtId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
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

-- setDirectAuth :: Id RegistrationToken -> SqlDB ()
-- setDirectAuth rtId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RegistrationTokenUpdatedAt =. val now,
--         RegistrationTokenVerified =. val True,
--         RegistrationTokenAuthMedium =. val SIGNATURE,
--         RegistrationTokenAuthType =. val DIRECT
--       ]
--     where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

setDirectAuth :: (L.MonadFlow m, MonadTime m) => Id RegistrationToken -> m (MeshResult ())
setDirectAuth (Id rtId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamRT.verified True,
          Se.Set BeamRT.authMedium SIGNATURE,
          Se.Set BeamRT.authType DIRECT,
          Se.Set BeamRT.updatedAt now
        ]
        [Se.Is BeamRT.id (Se.Eq rtId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
-- updateAttempts attemps rtId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RegistrationTokenUpdatedAt =. val now,
--         RegistrationTokenAttempts =. val attemps
--       ]
--     where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

updateAttempts :: (L.MonadFlow m, MonadTime m) => Int -> Id RegistrationToken -> m (MeshResult ())
updateAttempts attempts (Id rtId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
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

-- deleteByPersonId :: Id Person -> SqlDB ()
-- deleteByPersonId (Id personId) = do
--   delete $ do
--     registrationToken <- from $ table @RegistrationTokenT
--     where_ (registrationToken ^. RegistrationTokenEntityId ==. val personId)

deleteByPersonId :: L.MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.And [Se.Is BeamRT.entityId (Se.Eq personId)]]
    Nothing -> pure ()

-- deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
-- deleteByPersonIdExceptNew (Id personId) newRT = do
--   delete $ do
--     registrationToken <- from $ table @RegistrationTokenT
--     where_ $
--       (registrationToken ^. RegistrationTokenEntityId ==. val personId)
--         &&. not_ (registrationToken ^. RegistrationTokenId ==. val (getId newRT))

deleteByPersonIdExceptNew :: L.MonadFlow m => Id Person -> Id RegistrationToken -> m ()
deleteByPersonIdExceptNew (Id personId) (Id newRT) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.And [Se.Is BeamRT.entityId (Se.Eq personId), Se.Is BeamRT.id (Se.Not $ Se.Eq newRT)]]
    Nothing -> pure ()

-- findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
-- findAllByPersonId (Id personId) =
--   findAll $ do
--     registrationToken <- from $ table @RegistrationTokenT
--     where_ $ registrationToken ^. RegistrationTokenEntityId ==. val personId
--     return registrationToken

findAllByPersonId :: L.MonadFlow m => Id Person -> m [RegistrationToken]
findAllByPersonId personId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRT.RegistrationTokenT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamRegistrationTokenToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRT.entityId $ Se.Eq $ getId personId]
    Nothing -> pure []

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
      info = info
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
      BeamRT.info = info
    }
