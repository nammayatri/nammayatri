{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RiderDetails where

import Domain.Types.DriverReferral
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.RiderDetails as DRDD
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDetails as BeamRD

create :: L.MonadFlow m => DRDD.RiderDetails -> m (MeshResult ())
create riderDetails = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRD.RiderDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainRiderDetailsToBeam riderDetails)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id RiderDetails -> m (Maybe RiderDetails)
findById (Id riderDetailsId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRD.RiderDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRiderDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRD.id $ Se.Eq riderDetailsId]
    Nothing -> pure Nothing

findByMobileNumberAndMerchant :: (L.MonadFlow m, EncFlow m r) => Text -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberAndMerchant mobileNumber_ merchantId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRD.RiderDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  mobileNumberDbHash <- getDbHash mobileNumber_
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRiderDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq (getId merchantId)]]
    Nothing -> pure Nothing

updateHasTakenValidRide :: (L.MonadFlow m, MonadTime m) => Id RiderDetails -> m (MeshResult ())
updateHasTakenValidRide (Id riderId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRD.RiderDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamRD.hasTakenValidRide True,
          Se.Set BeamRD.hasTakenValidRideAt (Just now),
          Se.Set BeamRD.updatedAt now
        ]
        [Se.Is BeamRD.id (Se.Eq riderId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findAllReferredByDriverId :: L.MonadFlow m => Id Person -> m [RiderDetails]
findAllReferredByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRD.RiderDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamRiderDetailsToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamRD.referredByDriver $ Se.Eq (Just driverId)]
    Nothing -> pure []

findByMobileNumberHashAndMerchant :: L.MonadFlow m => DbHash -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberHashAndMerchant mobileNumberDbHash (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRD.RiderDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRiderDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.id $ Se.Eq merchantId]]
    Nothing -> pure Nothing

updateReferralInfo :: (L.MonadFlow m, MonadTime m) => DbHash -> Id Merchant -> Id DriverReferral -> Id Person -> m (MeshResult ())
updateReferralInfo customerNumberHash merchantId referralId driverId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamRD.RiderDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamRD.referralCode (Just $ getId referralId),
          Se.Set BeamRD.referredByDriver (Just $ getId driverId),
          Se.Set BeamRD.referredAt (Just now)
        ]
        [Se.And [Se.Is BeamRD.mobileNumberHash (Se.Eq customerNumberHash), Se.Is BeamRD.merchantId (Se.Eq $ getId merchantId)]]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamRiderDetailsToDomain :: BeamRD.RiderDetails -> RiderDetails
transformBeamRiderDetailsToDomain BeamRD.RiderDetailsT {..} = do
  RiderDetails
    { id = Id id,
      mobileCountryCode = mobileCountryCode,
      mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
      createdAt = createdAt,
      updatedAt = updatedAt,
      referralCode = Id <$> referralCode,
      referredByDriver = Id <$> referredByDriver,
      referredAt = referredAt,
      hasTakenValidRide = hasTakenValidRide,
      hasTakenValidRideAt = hasTakenValidRideAt,
      merchantId = Id merchantId
    }

transformDomainRiderDetailsToBeam :: RiderDetails -> BeamRD.RiderDetails
transformDomainRiderDetailsToBeam RiderDetails {..} =
  BeamRD.defaultRiderDetails
    { BeamRD.id = getId id,
      BeamRD.mobileCountryCode = mobileCountryCode,
      BeamRD.mobileNumberEncrypted = unEncrypted mobileNumber.encrypted,
      BeamRD.mobileNumberHash = mobileNumber.hash,
      BeamRD.createdAt = createdAt,
      BeamRD.updatedAt = updatedAt,
      BeamRD.referralCode = getId <$> referralCode,
      BeamRD.referredByDriver = getId <$> referredByDriver,
      BeamRD.referredAt = referredAt,
      BeamRD.hasTakenValidRide = hasTakenValidRide,
      BeamRD.hasTakenValidRideAt = hasTakenValidRideAt,
      BeamRD.merchantId = getId merchantId
    }
