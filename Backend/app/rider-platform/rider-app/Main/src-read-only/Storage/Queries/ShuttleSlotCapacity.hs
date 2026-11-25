{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ShuttleSlotCapacity where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ShuttleSlotCapacity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ShuttleSlotCapacity as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity] -> m ()
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m [Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity]
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByTripIdAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity))
findByTripIdAndMerchantOperatingCityId tripId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.tripId $ Se.Eq tripId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

updateCapacityById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Int -> (Kernel.Types.Id.Id Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity -> m ())
updateCapacityById capacity id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.capacity capacity, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity ->
  m (Maybe Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity -> m ()
updateByPrimaryKey (Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.capacity capacity,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.tripId tripId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ShuttleSlotCapacity Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity where
  fromTType' (Beam.ShuttleSlotCapacityT {..}) = do
    pure $
      Just
        Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity
          { capacity = capacity,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            tripId = tripId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ShuttleSlotCapacity Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity where
  toTType' (Domain.Types.ShuttleSlotCapacity.ShuttleSlotCapacity {..}) = do
    Beam.ShuttleSlotCapacityT
      { Beam.capacity = capacity,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.tripId = tripId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
