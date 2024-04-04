{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LocationMapping (module Storage.Queries.LocationMapping, module ReExport) where

import qualified Domain.Types.LocationMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LocationMapping as Beam
import Storage.Queries.LocationMappingExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.LocationMapping.LocationMapping -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.LocationMapping.LocationMapping] -> m ())
createMany = traverse_ create

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LocationMapping.LocationMapping -> m (Maybe Domain.Types.LocationMapping.LocationMapping))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.LocationMapping.LocationMapping -> m ())
updateByPrimaryKey (Domain.Types.LocationMapping.LocationMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.locationId (Kernel.Types.Id.getId locationId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.order order,
      Se.Set Beam.tag tag,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.version version
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
