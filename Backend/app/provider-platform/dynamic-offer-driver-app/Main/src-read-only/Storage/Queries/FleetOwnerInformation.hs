{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOwnerInformation (module Storage.Queries.FleetOwnerInformation, module ReExport) where

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
import Storage.Queries.FleetOwnerInformationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOwnerInformation.FleetOwnerInformation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOwnerInformation.FleetOwnerInformation] -> m ())
createMany = traverse_ create

deleteByFleetOwnerPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByFleetOwnerPersonId fleetOwnerPersonId = do deleteWithKV [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

findAllByPrimaryKeys :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.Person.Person] -> m [Domain.Types.FleetOwnerInformation.FleetOwnerInformation])
findAllByPrimaryKeys fleetOwnerPersonId = do findAllWithKV [Se.Is Beam.fleetOwnerPersonId $ Se.In (Kernel.Types.Id.getId <$> fleetOwnerPersonId)]

updateBusinessLicenseImage :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateBusinessLicenseImage businessLicenseImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.businessLicenseImageId businessLicenseImageId, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateFleetOwnerEnabledStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFleetOwnerEnabledStatus enabled fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateFleetOwnerVerifiedStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFleetOwnerVerifiedStatus verified fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.verified verified, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

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
