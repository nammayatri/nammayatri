{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedCabSlotCapacity where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SharedCabSlotCapacity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedCabSlotCapacity as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity] -> m ()
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m [Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity]
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByRouteIdAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity))
findByRouteIdAndMerchantOperatingCityId routeId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.routeId $ Se.Eq routeId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

updateCapacityById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Int -> (Kernel.Types.Id.Id Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity -> m ())
updateCapacityById capacity id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.capacity capacity, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity ->
  m (Maybe Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity -> m ()
updateByPrimaryKey (Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.capacity capacity,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.routeId routeId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedCabSlotCapacity Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity where
  fromTType' (Beam.SharedCabSlotCapacityT {..}) = do
    pure $
      Just
        Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity
          { capacity = capacity,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeId = routeId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SharedCabSlotCapacity Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity where
  toTType' (Domain.Types.SharedCabSlotCapacity.SharedCabSlotCapacity {..}) = do
    Beam.SharedCabSlotCapacityT
      { Beam.capacity = capacity,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeId = routeId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
