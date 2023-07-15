module Storage.Queries.Sos where

import Domain.Types.Person as Person ()
import Domain.Types.Sos as Sos
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Sos as BeamS

create :: L.MonadFlow m => Sos.Sos -> m (MeshResult ())
create sos = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamS.SosT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainSosToBeam sos)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- updateStatus :: Id Sos.Sos -> Sos.SosStatus -> SqlDB ()
-- updateStatus sosId status = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ SosUpdatedAt =. val now,
--         SosStatus =. val status
--       ]
--     where_ $ tbl ^. SosId ==. val (getId sosId)

updateStatus :: (L.MonadFlow m, MonadTime m) => Id Sos.Sos -> Sos.SosStatus -> m (MeshResult ())
updateStatus sosId status = do
  now <- getCurrentTime
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamS.SosT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' ->
      KV.updateWoReturningWithKVConnector
        dbCOnf'
        updatedMeshConfig
        [ Se.Set BeamS.status status,
          Se.Set BeamS.updatedAt now
        ]
        [Se.Is BeamS.id $ Se.Eq (getId sosId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- findById :: Transactionable m => Id Sos.Sos -> m (Maybe Sos)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id Sos.Sos -> m (Maybe Sos)
findById sosId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamS.SosT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamSosToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamS.id $ Se.Eq (getId sosId)]
    Nothing -> pure Nothing

transformBeamSosToDomain :: BeamS.Sos -> Sos
transformBeamSosToDomain BeamS.SosT {..} = do
  Sos
    { id = Id id,
      personId = Id personId,
      rideId = Id rideId,
      status = status,
      flow = flow,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainSosToBeam :: Sos -> BeamS.Sos
transformDomainSosToBeam Sos {..} =
  BeamS.SosT
    { BeamS.id = getId id,
      BeamS.personId = getId personId,
      BeamS.rideId = getId rideId,
      BeamS.status = status,
      BeamS.flow = flow,
      BeamS.createdAt = createdAt,
      BeamS.updatedAt = updatedAt
    }
