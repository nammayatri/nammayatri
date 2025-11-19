{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Volunteer (module Storage.Queries.Volunteer, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.Volunteer
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Volunteer as Beam
import Storage.Queries.VolunteerExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Volunteer.Volunteer -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Volunteer.Volunteer] -> m ())
createMany = traverse_ create

findAllByPlace :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m [Domain.Types.Volunteer.Volunteer])
findAllByPlace place = do findAllWithKV [Se.Is Beam.place $ Se.Eq place]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Volunteer.Volunteer -> m (Maybe Domain.Types.Volunteer.Volunteer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByIdAndVendorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Volunteer.Volunteer -> Kernel.Prelude.Maybe Data.Text.Text -> m (Maybe Domain.Types.Volunteer.Volunteer))
findByIdAndVendorId id vendorId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.vendorId $ Se.Eq (Kernel.Prelude.fromMaybe "DEFAULT_VENDOR" vendorId)]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Volunteer.Volunteer -> Kernel.Prelude.Maybe Data.Text.Text -> m (Maybe Domain.Types.Volunteer.Volunteer))
findByPrimaryKey id vendorId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.vendorId $ Se.Eq (Kernel.Prelude.fromMaybe "DEFAULT_VENDOR" vendorId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Volunteer.Volunteer -> m ())
updateByPrimaryKey (Domain.Types.Volunteer.Volunteer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isActive ((Kernel.Prelude.Just . Kernel.Prelude.fromMaybe True) isActive),
      Se.Set Beam.place place,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.vendorId $ Se.Eq (Kernel.Prelude.fromMaybe "DEFAULT_VENDOR" vendorId)
        ]
    ]
