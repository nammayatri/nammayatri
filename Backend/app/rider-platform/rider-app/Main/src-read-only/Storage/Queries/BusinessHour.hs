{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BusinessHour (module Storage.Queries.BusinessHour, module ReExport) where

import qualified Domain.Types.BusinessHour
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessHour as Beam
import Storage.Queries.BusinessHourExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BusinessHour.BusinessHour -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BusinessHour.BusinessHour] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByHash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.BusinessHour.BusinessHour])
findAllByHash hash = do findAllWithKV [Se.Is Beam.hash $ Se.Eq hash]

findAllByPlaceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.BusinessHour.BusinessHour])
findAllByPlaceId placeId = do findAllWithKV [Se.Is Beam.placeId $ Se.Eq placeId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m (Maybe Domain.Types.BusinessHour.BusinessHour))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m (Maybe Domain.Types.BusinessHour.BusinessHour))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BusinessHour.BusinessHour -> m ())
updateByPrimaryKey (Domain.Types.BusinessHour.BusinessHour {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingClosingTime bookingClosingTime,
      Se.Set Beam.btype btype,
      Se.Set Beam.categoryId (Kernel.Types.Id.getId <$> categoryId),
      Se.Set Beam.expiryDate expiryDate,
      Se.Set Beam.hash hash,
      Se.Set Beam.name name,
      Se.Set Beam.placeId placeId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
