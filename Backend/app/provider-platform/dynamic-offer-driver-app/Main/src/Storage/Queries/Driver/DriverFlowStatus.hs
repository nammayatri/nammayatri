{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Driver.DriverFlowStatus where

import qualified Debug.Trace as T
import Domain.Types.Driver.DriverFlowStatus
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.Person
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Types (MeshConfig (..))
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Driver.DriverFlowStatus as BeamDFS

updatedMeshCfg :: MeshConfig
updatedMeshCfg = Mesh.meshConfig {meshEnabled = True, kvHardKilled = False}

create :: L.MonadFlow m => DDFS.DriverFlowStatus -> m (MeshResult ())
create driverFlowStatus = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> T.trace ("Create started here") $ KV.createWoReturingKVConnector dbConf' updatedMeshCfg (transformDomainDriverFlowStatusToBeam driverFlowStatus)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshCfg
          [Se.Is BeamDFS.personId (Se.Eq driverId)]
    Nothing -> pure ()

getStatus :: L.MonadFlow m => Id Person -> m (Maybe DDFS.FlowStatus)
getStatus (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      dfsData <- KV.findWithKVConnector dbConf' updatedMeshCfg [Se.Is BeamDFS.personId $ Se.Eq personId]
      case dfsData of
        Left _ -> pure Nothing
        Right x -> do
          let dfsData' = transformBeamDriverFlowStatusToDomain <$> x
          let fs = DDFS.flowStatus <$> dfsData'
          pure fs
    Nothing -> pure Nothing

updateStatus :: (L.MonadFlow m, MonadTime m) => Id Person -> DDFS.FlowStatus -> m ()
updateStatus (Id personId) flowStatus = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshCfg
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
