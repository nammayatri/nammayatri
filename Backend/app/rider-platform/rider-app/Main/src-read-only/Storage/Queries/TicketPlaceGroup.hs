{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketPlaceGroup where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlaceGroup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketPlaceGroup as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketPlaceGroup.TicketPlaceGroup -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketPlaceGroup.TicketPlaceGroup] -> m ()
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m [Domain.Types.TicketPlaceGroup.TicketPlaceGroup]
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketPlaceGroup.TicketPlaceGroup -> m (Maybe Domain.Types.TicketPlaceGroup.TicketPlaceGroup)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketPlaceGroup.TicketPlaceGroup -> m (Maybe Domain.Types.TicketPlaceGroup.TicketPlaceGroup)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketPlaceGroup.TicketPlaceGroup -> m ()
updateByPrimaryKey (Domain.Types.TicketPlaceGroup.TicketPlaceGroup {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.geomGeoJson geomGeoJson,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketPlaceGroup Domain.Types.TicketPlaceGroup.TicketPlaceGroup where
  fromTType' (Beam.TicketPlaceGroupT {..}) = do
    pure $
      Just
        Domain.Types.TicketPlaceGroup.TicketPlaceGroup
          { description = description,
            geomGeoJson = geomGeoJson,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketPlaceGroup Domain.Types.TicketPlaceGroup.TicketPlaceGroup where
  toTType' (Domain.Types.TicketPlaceGroup.TicketPlaceGroup {..}) = do
    Beam.TicketPlaceGroupT
      { Beam.description = description,
        Beam.geomGeoJson = geomGeoJson,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
