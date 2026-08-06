{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverGroupInsurance (module Storage.Queries.DriverGroupInsurance, module ReExport) where

import qualified Domain.Types.DriverGroupInsurance
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverGroupInsurance as Beam
import Storage.Queries.DriverGroupInsuranceExtra as ReExport
import Storage.Queries.OrphanInstances.DriverGroupInsurance ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGroupInsurance.DriverGroupInsurance -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverGroupInsurance.DriverGroupInsurance] -> m ())
createMany = traverse_ create

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.DriverGroupInsurance.DriverGroupInsurance]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByDriverIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DriverGroupInsurance.DriverGroupInsuranceType -> m (Maybe Domain.Types.DriverGroupInsurance.DriverGroupInsurance))
findByDriverIdAndType driverId insuranceType = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.insuranceType $ Se.Eq insuranceType]]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverGroupInsurance.DriverGroupInsurance -> m (Maybe Domain.Types.DriverGroupInsurance.DriverGroupInsurance))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateEnabledAtAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Domain.Types.DriverGroupInsurance.DriverGroupInsuranceStatus -> Kernel.Types.Id.Id Domain.Types.DriverGroupInsurance.DriverGroupInsurance -> m ())
updateEnabledAtAndStatus enabledAt status id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.enabledAt enabledAt, Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateSecondBotCheckAt ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.DriverGroupInsurance.DriverGroupInsurance -> m ())
updateSecondBotCheckAt secondBotCheckAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.secondBotCheckAt secondBotCheckAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DriverGroupInsurance.DriverGroupInsuranceStatus -> Kernel.Types.Id.Id Domain.Types.DriverGroupInsurance.DriverGroupInsurance -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverGroupInsurance.DriverGroupInsurance -> m (Maybe Domain.Types.DriverGroupInsurance.DriverGroupInsurance))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGroupInsurance.DriverGroupInsurance -> m ())
updateByPrimaryKey (Domain.Types.DriverGroupInsurance.DriverGroupInsurance {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.age age,
      Se.Set Beam.dob dob,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.enabledAt enabledAt,
      Se.Set Beam.fullName fullName,
      Se.Set Beam.gender gender,
      Se.Set Beam.insuranceType insuranceType,
      Se.Set Beam.lastExportedAt lastExportedAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.mobile mobile,
      Se.Set Beam.nomineeDob nomineeDob,
      Se.Set Beam.nomineeName nomineeName,
      Se.Set Beam.nomineeRelationship nomineeRelationship,
      Se.Set Beam.secondBotCheckAt secondBotCheckAt,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
