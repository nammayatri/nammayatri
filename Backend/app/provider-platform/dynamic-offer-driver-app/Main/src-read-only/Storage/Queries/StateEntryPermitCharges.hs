{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StateEntryPermitCharges (module Storage.Queries.StateEntryPermitCharges, module ReExport) where

import qualified Domain.Types.Geometry
import qualified Domain.Types.StateEntryPermitCharges
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.StateEntryPermitCharges as Beam
import Storage.Queries.StateEntryPermitChargesExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges] -> m ())
createMany = traverse_ create

findAllByGeomId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Geometry.Geometry -> m ([Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges]))
findAllByGeomId geomId = do findAllWithKV [Se.Is Beam.geomId $ Se.Eq (Kernel.Types.Id.getId geomId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges -> m (Maybe Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges -> m ())
updateByPrimaryKey (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.geomId (Kernel.Types.Id.getId geomId),
      Se.Set Beam.name name,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
