{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EDCMachineMapping (module Storage.Queries.EDCMachineMapping, module ReExport) where

import qualified Domain.Types.EDCMachineMapping
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EDCMachineMapping as Beam
import Storage.Queries.EDCMachineMappingExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EDCMachineMapping.EDCMachineMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.EDCMachineMapping.EDCMachineMapping] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> m (Maybe Domain.Types.EDCMachineMapping.EDCMachineMapping))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.EDCMachineMapping.EDCMachineMapping))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> m (Maybe Domain.Types.EDCMachineMapping.EDCMachineMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EDCMachineMapping.EDCMachineMapping -> m ())
updateByPrimaryKey (Domain.Types.EDCMachineMapping.EDCMachineMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientId clientId,
      Se.Set Beam.createdBy (Kernel.Types.Id.getId <$> createdBy),
      Se.Set Beam.isActive isActive,
      Se.Set Beam.machineName machineName,
      Se.Set Beam.merchantChannelId merchantChannelId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantKey merchantKey,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paytmMid paytmMid,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.terminalId terminalId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
