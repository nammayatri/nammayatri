{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.AadhaarVerification where

import Domain.Types.DriverOnboarding.AadhaarVerification
import Domain.Types.Person (Person)
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.AadhaarVerification as BeamAV

-- create :: AadhaarVerification -> Esq.SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => AadhaarVerification -> m (MeshResult ())
create aadhaarVerification = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamAV.AadhaarVerificationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainAadhaarVerificationToBeam aadhaarVerification)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById ::
--   Transactionable m =>
--   Id AadhaarVerification ->
--   m (Maybe AadhaarVerification)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id AadhaarVerification -> m (Maybe AadhaarVerification)
findById (Id aadhaarVerification) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamAV.AadhaarVerificationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamAadhaarVerificationToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamAV.id $ Se.Eq aadhaarVerification]
    Nothing -> pure Nothing

-- findByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m (Maybe AadhaarVerification)
-- findByDriverId driverId = do
--   findOne $ do
--     aadhaar <- from $ table @AadhaarVerificationT
--     where_ $ aadhaar ^. AadhaarVerificationDriverId ==. val (toKey driverId)
--     return aadhaar

findByDriverId :: L.MonadFlow m => Id Person -> m (Maybe AadhaarVerification)
findByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamAV.AadhaarVerificationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamAadhaarVerificationToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamAV.driverId $ Se.Eq driverId]
    Nothing -> pure Nothing

transformBeamAadhaarVerificationToDomain :: BeamAV.AadhaarVerification -> AadhaarVerification
transformBeamAadhaarVerificationToDomain BeamAV.AadhaarVerificationT {..} = do
  AadhaarVerification
    { id = Id id,
      driverId = Id driverId,
      driverName = driverName,
      driverGender = driverGender,
      driverDob = driverDob,
      driverImage = driverImage,
      createdAt = createdAt
    }

transformDomainAadhaarVerificationToBeam :: AadhaarVerification -> BeamAV.AadhaarVerification
transformDomainAadhaarVerificationToBeam AadhaarVerification {..} =
  BeamAV.AadhaarVerificationT
    { BeamAV.id = getId id,
      BeamAV.driverId = getId driverId,
      BeamAV.driverName = driverName,
      BeamAV.driverGender = driverGender,
      BeamAV.driverDob = driverDob,
      BeamAV.driverImage = driverImage,
      BeamAV.createdAt = createdAt
    }
