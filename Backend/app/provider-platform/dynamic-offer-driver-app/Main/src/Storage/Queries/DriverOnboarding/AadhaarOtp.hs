{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.AadhaarOtp where

import Domain.Types.DriverOnboarding.AadhaarOtp
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.AadhaarOtpReq as BeamAOR
import qualified Storage.Beam.DriverOnboarding.AadhaarOtpVerify as BeamAOV
import Storage.Tabular.DriverOnboarding.AadhaarOtpReq ()
import Storage.Tabular.DriverOnboarding.AadhaarOtpVerify ()

createForGenerate :: L.MonadFlow m => AadhaarOtpReq -> m (MeshResult ())
createForGenerate aadhaarOtpReq = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamAOR.AadhaarOtpReqT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainAadhaarOtpReqToBeam aadhaarOtpReq)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

createForVerify :: L.MonadFlow m => AadhaarOtpVerify -> m (MeshResult ())
createForVerify aadhaarOtpVerify = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamAOV.AadhaarOtpVerifyT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainAadhaarOtpVerifyToBeam aadhaarOtpVerify)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

transformBeamAadhaarOtpReqToDomain :: BeamAOR.AadhaarOtpReq -> AadhaarOtpReq
transformBeamAadhaarOtpReqToDomain BeamAOR.AadhaarOtpReqT {..} = do
  AadhaarOtpReq
    { id = Id id,
      driverId = Id driverId,
      requestId = requestId,
      statusCode = statusCode,
      transactionId = transactionId,
      requestMessage = requestMessage,
      createdAt = createdAt
    }

transformDomainAadhaarOtpReqToBeam :: AadhaarOtpReq -> BeamAOR.AadhaarOtpReq
transformDomainAadhaarOtpReqToBeam AadhaarOtpReq {..} =
  BeamAOR.defaultAadhaarOtpReq
    { BeamAOR.id = getId id,
      BeamAOR.driverId = getId driverId,
      BeamAOR.requestId = requestId,
      BeamAOR.statusCode = statusCode,
      BeamAOR.transactionId = transactionId,
      BeamAOR.requestMessage = requestMessage,
      BeamAOR.createdAt = createdAt
    }

transformBeamAadhaarOtpVerifyToDomain :: BeamAOV.AadhaarOtpVerify -> AadhaarOtpVerify
transformBeamAadhaarOtpVerifyToDomain BeamAOV.AadhaarOtpVerifyT {..} = do
  AadhaarOtpVerify
    { id = Id id,
      driverId = Id driverId,
      requestId = requestId,
      statusCode = statusCode,
      transactionId = transactionId,
      requestMessage = requestMessage,
      createdAt = createdAt
    }

transformDomainAadhaarOtpVerifyToBeam :: AadhaarOtpVerify -> BeamAOV.AadhaarOtpVerify
transformDomainAadhaarOtpVerifyToBeam AadhaarOtpVerify {..} =
  BeamAOV.defaultAadhaarOtpVerify
    { BeamAOV.id = getId id,
      BeamAOV.driverId = getId driverId,
      BeamAOV.requestId = requestId,
      BeamAOV.statusCode = statusCode,
      BeamAOV.transactionId = transactionId,
      BeamAOV.requestMessage = requestMessage,
      BeamAOV.createdAt = createdAt
    }
