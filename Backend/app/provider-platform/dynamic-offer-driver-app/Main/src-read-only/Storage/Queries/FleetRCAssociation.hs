{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetRCAssociation where

import qualified Domain.Types.DriverRCAssociation
import qualified Domain.Types.FleetRCAssociation
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRCAssociation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRCAssociation.FleetRCAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetRCAssociation.FleetRCAssociation] -> m ())
createMany = traverse_ create

deactivateRCForDriver ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
deactivateRCForDriver isRcActive (Kernel.Types.Id.Id driverId) (Kernel.Types.Id.Id rcId) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.isRcActive isRcActive, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.rcId $ Se.Eq rcId]]

deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findActiveAssociationByDriver ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findActiveAssociationByDriver (Kernel.Types.Id.Id driverId) isRcActive = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.isRcActive $ Se.Eq isRcActive]]

findActiveAssociationByRC ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findActiveAssociationByRC (Kernel.Types.Id.Id rcId) isRcActive = do findOneWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.isRcActive $ Se.Eq isRcActive]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverRCAssociation.DriverRCAssociation -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverRCAssociation.DriverRCAssociation -> m (Maybe Domain.Types.FleetRCAssociation.FleetRCAssociation))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRCAssociation.FleetRCAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetRCAssociation.FleetRCAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.isRcActive isRcActive,
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.rcVerificationStatus rcVerificationStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FleetRCAssociation Domain.Types.FleetRCAssociation.FleetRCAssociation where
  fromTType' (Beam.FleetRCAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetRCAssociation.FleetRCAssociation
          { associatedOn = associatedOn,
            associatedTill = associatedTill,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            isRcActive = isRcActive,
            rcId = Kernel.Types.Id.Id rcId,
            rcVerificationStatus = rcVerificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetRCAssociation Domain.Types.FleetRCAssociation.FleetRCAssociation where
  toTType' (Domain.Types.FleetRCAssociation.FleetRCAssociation {..}) = do
    Beam.FleetRCAssociationT
      { Beam.associatedOn = associatedOn,
        Beam.associatedTill = associatedTill,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isRcActive = isRcActive,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.rcVerificationStatus = rcVerificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
