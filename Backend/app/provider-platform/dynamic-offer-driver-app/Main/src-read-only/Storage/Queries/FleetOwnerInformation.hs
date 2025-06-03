{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOwnerInformation where

import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerInformation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOwnerInformation.FleetOwnerInformation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOwnerInformation.FleetOwnerInformation] -> m ())
createMany = traverse_ create

updateAadhaarImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAadhaarImage aadhaarNumber aadhaarFrontImageId aadhaarBackImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.aadhaarNumber aadhaarNumber,
      Se.Set Beam.aadhaarFrontImageId aadhaarFrontImageId,
      Se.Set Beam.aadhaarBackImageId aadhaarBackImageId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateBusinessLicenseImage :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateBusinessLicenseImage businessLicenseImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.businessLicenseImageId businessLicenseImageId, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateBusinessLicenseImageAndNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateBusinessLicenseImageAndNumber businessLicenseImageId businessLicenseNumber fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.businessLicenseImageId businessLicenseImageId,
      Se.Set Beam.businessLicenseNumber businessLicenseNumber,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateFleetOwnerEnabledStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFleetOwnerEnabledStatus enabled fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateFleetOwnerGstNumberAndEnabledStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFleetOwnerGstNumberAndEnabledStatus gstNumber enabled fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.gstNumber gstNumber, Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateFleetOwnerVerifiedStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFleetOwnerVerifiedStatus verified fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verified verified, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateGstImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateGstImage gstNumber gstImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.gstNumber gstNumber, Se.Set Beam.gstImageId gstImageId, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updatePanImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePanImage panNumber panImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.panNumber panNumber, Se.Set Beam.panImageId panImageId, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateReferredByOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferredByOperatorId referredByOperatorId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referredByOperatorId referredByOperatorId, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateRegistration :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateRegistration registeredAt fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.registeredAt registeredAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.FleetOwnerInformation.FleetOwnerInformation))
findByPrimaryKey fleetOwnerPersonId = do findOneWithKV [Se.And [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOwnerInformation.FleetOwnerInformation -> m ())
updateByPrimaryKey (Domain.Types.FleetOwnerInformation.FleetOwnerInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadhaarBackImageId aadhaarBackImageId,
      Se.Set Beam.aadhaarFrontImageId aadhaarFrontImageId,
      Se.Set Beam.aadhaarNumber aadhaarNumber,
      Se.Set Beam.blocked blocked,
      Se.Set Beam.businessLicenseImageId businessLicenseImageId,
      Se.Set Beam.businessLicenseNumber businessLicenseNumber,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.fleetType fleetType,
      Se.Set Beam.gstImageId gstImageId,
      Se.Set Beam.gstNumber gstNumber,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.panImageId panImageId,
      Se.Set Beam.panNumber panNumber,
      Se.Set Beam.referredByOperatorId referredByOperatorId,
      Se.Set Beam.registeredAt registeredAt,
      Se.Set Beam.verified verified,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]]

instance FromTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation where
  fromTType' (Beam.FleetOwnerInformationT {..}) = do
    pure $
      Just
        Domain.Types.FleetOwnerInformation.FleetOwnerInformation
          { aadhaarBackImageId = aadhaarBackImageId,
            aadhaarFrontImageId = aadhaarFrontImageId,
            aadhaarNumber = aadhaarNumber,
            blocked = blocked,
            businessLicenseImageId = businessLicenseImageId,
            businessLicenseNumber = businessLicenseNumber,
            enabled = enabled,
            fleetOwnerPersonId = Kernel.Types.Id.Id fleetOwnerPersonId,
            fleetType = fleetType,
            gstImageId = gstImageId,
            gstNumber = gstNumber,
            merchantId = Kernel.Types.Id.Id merchantId,
            panImageId = panImageId,
            panNumber = panNumber,
            referredByOperatorId = referredByOperatorId,
            registeredAt = registeredAt,
            verified = verified,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation where
  toTType' (Domain.Types.FleetOwnerInformation.FleetOwnerInformation {..}) = do
    Beam.FleetOwnerInformationT
      { Beam.aadhaarBackImageId = aadhaarBackImageId,
        Beam.aadhaarFrontImageId = aadhaarFrontImageId,
        Beam.aadhaarNumber = aadhaarNumber,
        Beam.blocked = blocked,
        Beam.businessLicenseImageId = businessLicenseImageId,
        Beam.businessLicenseNumber = businessLicenseNumber,
        Beam.enabled = enabled,
        Beam.fleetOwnerPersonId = Kernel.Types.Id.getId fleetOwnerPersonId,
        Beam.fleetType = fleetType,
        Beam.gstImageId = gstImageId,
        Beam.gstNumber = gstNumber,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.panImageId = panImageId,
        Beam.panNumber = panNumber,
        Beam.referredByOperatorId = referredByOperatorId,
        Beam.registeredAt = registeredAt,
        Beam.verified = verified,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
