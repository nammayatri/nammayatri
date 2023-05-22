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
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDetails as BeamRD
import Storage.Tabular.RiderDetails
import qualified Storage.Tabular.VechileNew as VN

create :: RiderDetails -> SqlDB ()
create = Esq.create

create' :: L.MonadFlow m => DRDD.RiderDetails -> m (MeshResult ())
create' riderDetails = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainRiderDetailsToBeam riderDetails)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- TODO :: write cached query for this
findById ::
  Transactionable m =>
  Id RiderDetails ->
  m (Maybe RiderDetails)
findById = Esq.findById

findById' :: L.MonadFlow m => Id RiderDetails -> m (Maybe RiderDetails)
findById' (Id riderDetailsId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRiderDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamRD.id $ Se.Eq riderDetailsId]
    Nothing -> pure Nothing

findByMobileNumberAndMerchant ::
  (MonadThrow m, Log m, Transactionable m, EncFlow m r) =>
  Text ->
  Id Merchant ->
  m (Maybe RiderDetails)
findByMobileNumberAndMerchant mobileNumber_ merchantId = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  Esq.findOne $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $
      riderDetails ^. RiderDetailsMobileNumberHash ==. val mobileNumberDbHash
        &&. riderDetails ^. RiderDetailsMerchantId ==. val (toKey merchantId)
    return riderDetails

findByMobileNumberAndMerchant' :: (L.MonadFlow m, EncFlow m r) => Text -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberAndMerchant' mobileNumber_ merchantId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  mobileNumberDbHash <- getDbHash mobileNumber_
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRiderDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq (getId merchantId)]]
    Nothing -> pure Nothing

updateHasTakenValidRide :: Id RiderDetails -> SqlDB ()
updateHasTakenValidRide riderId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RiderDetailsHasTakenValidRide =. val True,
        RiderDetailsUpdatedAt =. val now,
        RiderDetailsHasTakenValidRideAt =. val (Just now)
      ]
    where_ $ tbl ^. RiderDetailsTId ==. val (toKey riderId)

updateHasTakenValidRide' :: (L.MonadFlow m, MonadTime m) => Id RiderDetails -> m (MeshResult ())
updateHasTakenValidRide' (Id riderId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamRD.hasTakenValidRide True,
          Se.Set BeamRD.hasTakenValidRideAt (Just now),
          Se.Set BeamRD.updatedAt now
        ]
        [Se.Is BeamRD.id (Se.Eq riderId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findAllReferredByDriverId :: Transactionable m => Id Person -> m [RiderDetails]
findAllReferredByDriverId driverId = do
  Esq.findAll $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $ riderDetails ^. RiderDetailsReferredByDriver ==. val (Just $ toKey driverId)
    return riderDetails

findAllReferredByDriverId' :: L.MonadFlow m => Id Person -> m [RiderDetails]
findAllReferredByDriverId' (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamRiderDetailsToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamRD.referredByDriver $ Se.Eq (Just driverId)]
    Nothing -> pure []

findByMobileNumberHashAndMerchant :: Transactionable m => DbHash -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberHashAndMerchant mobileNumberDbHash merchantId = do
  Esq.findOne $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $
      riderDetails ^. RiderDetailsMobileNumberHash ==. val mobileNumberDbHash
        &&. riderDetails ^. RiderDetailsMerchantId ==. val (toKey merchantId)
    return riderDetails

findByMobileNumberHashAndMerchant' :: L.MonadFlow m => DbHash -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberHashAndMerchant' mobileNumberDbHash (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRiderDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.id $ Se.Eq merchantId]]
    Nothing -> pure Nothing

updateReferralInfo ::
  DbHash ->
  Id Merchant ->
  Id DriverReferral ->
  Id Person ->
  SqlDB ()
updateReferralInfo customerNumberHash merchantId referralId driverId = do
  now <- getCurrentTime
  Esq.update $ \rd -> do
    set
      rd
      [ RiderDetailsReferralCode =. val (Just $ toKey referralId),
        RiderDetailsReferredByDriver =. val (Just $ toKey driverId),
        RiderDetailsReferredAt =. val (Just now)
      ]
    where_ $
      rd ^. RiderDetailsMobileNumberHash ==. val customerNumberHash
        &&. rd ^. RiderDetailsMerchantId ==. val (toKey merchantId)

updateDriverResponse' :: (L.MonadFlow m, MonadTime m) => DbHash -> Id Merchant -> Id DriverReferral -> Id Person -> m (MeshResult ())
updateDriverResponse' customerNumberHash merchantId referralId driverId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
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
