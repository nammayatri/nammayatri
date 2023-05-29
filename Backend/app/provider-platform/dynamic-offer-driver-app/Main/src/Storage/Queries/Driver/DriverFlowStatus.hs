{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Driver.DriverFlowStatus where

import Domain.Types.Driver.DriverFlowStatus
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.Person
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Driver.DriverFlowStatus as BeamDFS

-- create :: DDFS.DriverFlowStatus -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DDFS.DriverFlowStatus -> m (MeshResult ())
create driverFlowStatus = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverFlowStatusToBeam driverFlowStatus)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- deleteById :: Id Person -> SqlDB ()
-- deleteById = Esq.deleteByKey @DriverFlowStatusT

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamDFS.personId (Se.Eq driverId)]
    Nothing -> pure ()

-- getStatus ::
--   (Transactionable m) =>
--   Id Person ->
--   m (Maybe DDFS.FlowStatus)
-- getStatus personId = do
--   findOne $ do
--     driverFlowStatus <- from $ table @DriverFlowStatusT
--     where_ $
--       driverFlowStatus ^. DriverFlowStatusTId ==. val (toKey personId)
--     return $ driverFlowStatus ^. DriverFlowStatusFlowStatus

getStatus :: L.MonadFlow m => Id Person -> m (Maybe DDFS.FlowStatus)
getStatus (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      dfsData <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamDFS.personId $ Se.Eq personId]
      case dfsData of
        Left _ -> pure Nothing
        Right x -> do
          let dfsData' = transformBeamDriverFlowStatusToDomain <$> x
          let fs = DDFS.flowStatus <$> dfsData'
          pure fs
    Nothing -> pure Nothing

-- updateStatus :: Id Person -> DDFS.FlowStatus -> SqlDB ()
-- updateStatus personId flowStatus = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverFlowStatusUpdatedAt =. val now,
--         DriverFlowStatusFlowStatus =. val flowStatus
--       ]
--     where_ $ tbl ^. DriverFlowStatusTId ==. val (toKey personId)

updateStatus :: (L.MonadFlow m, MonadTime m) => Id Person -> DDFS.FlowStatus -> m ()
updateStatus (Id personId) flowStatus = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.Set BeamDFS.flowStatus flowStatus,
            Se.Set BeamDFS.updatedAt now
          ]
          [Se.Is BeamDFS.personId $ Se.Eq personId]
    Nothing -> pure ()

transformBeamDriverFlowStatusToDomain :: BeamDFS.DriverFlowStatus -> DriverFlowStatus
transformBeamDriverFlowStatusToDomain BeamDFS.DriverFlowStatusT {..} = do
  DriverFlowStatus
    { personId = Id personId,
      flowStatus = flowStatus,
      updatedAt = updatedAt
    }

transformDomainDriverFlowStatusToBeam :: DriverFlowStatus -> BeamDFS.DriverFlowStatus
transformDomainDriverFlowStatusToBeam DriverFlowStatus {..} =
  BeamDFS.defaultDriverFlowStatus
    { BeamDFS.personId = getId personId,
      BeamDFS.flowStatus = flowStatus,
      BeamDFS.updatedAt = updatedAt
    }
