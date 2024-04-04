{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverRCAssociation (module Storage.Queries.DriverRCAssociation, module ReExport) where

import qualified Domain.Types.DriverRCAssociation
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverRCAssociation as Beam
import Storage.Queries.DriverRCAssociationExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.DriverRCAssociation.DriverRCAssociation -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.DriverRCAssociation.DriverRCAssociation] -> m ())
createMany = traverse_ create

deactivateRCForDriver ::
  KvDbFlow m r =>
  (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
deactivateRCForDriver isRcActive (Kernel.Types.Id.Id driverId) (Kernel.Types.Id.Id rcId) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.isRcActive isRcActive, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.rcId $ Se.Eq rcId]]

deleteByDriverId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId (Kernel.Types.Id.Id driverId) = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq driverId]

findActiveAssociationByDriver :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.DriverRCAssociation.DriverRCAssociation))
findActiveAssociationByDriver (Kernel.Types.Id.Id driverId) isRcActive = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.isRcActive $ Se.Eq isRcActive]]

findActiveAssociationByRC ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.DriverRCAssociation.DriverRCAssociation))
findActiveAssociationByRC (Kernel.Types.Id.Id rcId) isRcActive = do findOneWithKV [Se.And [Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.isRcActive $ Se.Eq isRcActive]]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverRCAssociation.DriverRCAssociation -> m (Maybe Domain.Types.DriverRCAssociation.DriverRCAssociation))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverRCAssociation.DriverRCAssociation -> m (Maybe Domain.Types.DriverRCAssociation.DriverRCAssociation))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.DriverRCAssociation.DriverRCAssociation -> m ())
updateByPrimaryKey (Domain.Types.DriverRCAssociation.DriverRCAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.associatedOn associatedOn,
      Se.Set Beam.associatedTill associatedTill,
      Se.Set Beam.consent consent,
      Se.Set Beam.consentTimestamp consentTimestamp,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.isRcActive isRcActive,
      Se.Set Beam.rcId (Kernel.Types.Id.getId rcId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
