{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ServiceCategory (module Storage.Queries.ServiceCategory, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ServiceCategory as Beam
import Storage.Queries.ServiceCategoryExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ServiceCategory.ServiceCategory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ServiceCategory.ServiceCategory] -> m ())
createMany = traverse_ create

findAllByPlaceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.ServiceCategory.ServiceCategory])
findAllByPlaceId placeId = do findAllWithKV [Se.Is Beam.placeId $ Se.Eq placeId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m (Maybe Domain.Types.ServiceCategory.ServiceCategory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m (Maybe Domain.Types.ServiceCategory.ServiceCategory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ServiceCategory.ServiceCategory -> m ())
updateByPrimaryKey (Domain.Types.ServiceCategory.ServiceCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowedSeats allowedSeats,
      Se.Set Beam.availableSeats availableSeats,
      Se.Set Beam.description description,
      Se.Set Beam.inclusionPoints (Data.Aeson.toJSON <$> inclusionPoints),
      Se.Set Beam.isClosed (Kernel.Prelude.Just isClosed),
      Se.Set Beam.maxSelection maxSelection,
      Se.Set Beam.name name,
      Se.Set Beam.peopleCategory (Kernel.Types.Id.getId <$> peopleCategory),
      Se.Set Beam.placeId placeId,
      Se.Set Beam.rules (Data.Aeson.toJSON <$> rules),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
