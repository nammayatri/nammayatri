{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.KioskLocation (module Storage.Queries.KioskLocation, module ReExport) where

import qualified Domain.Types.KioskLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.KioskLocation as Beam
import Storage.Queries.KioskLocationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KioskLocation.KioskLocation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.KioskLocation.KioskLocation] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KioskLocation.KioskLocation -> m (Maybe Domain.Types.KioskLocation.KioskLocation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KioskLocation.KioskLocation -> m ())
updateByPrimaryKey (Domain.Types.KioskLocation.KioskLocation {..}) = do
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.contact contact,
      Se.Set Beam.landmark landmark,
      Se.Set Beam.latitude latitude,
      Se.Set Beam.longitude longitude,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
