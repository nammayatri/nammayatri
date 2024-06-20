{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketPlace where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketPlace as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketPlace.TicketPlace -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketPlace.TicketPlace] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe Domain.Types.TicketPlace.TicketPlace))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

getTicketPlaces :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe Domain.Types.TicketPlace.TicketPlace))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketPlace.TicketPlace -> m ())
updateByPrimaryKey (Domain.Types.TicketPlace.TicketPlace {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.description description,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.gallery gallery,
      Se.Set Beam.shortDesc shortDesc,
      Se.Set Beam.iconUrl iconUrl,
      Se.Set Beam.termsAndConditions termsAndConditions,
      Se.Set Beam.placeType placeType,
      Se.Set Beam.mapImageUrl mapImageUrl,
      Se.Set Beam.openTimings openTimings,
      Se.Set Beam.closeTimings closeTimings,
      Se.Set Beam.status status,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  fromTType' (Beam.TicketPlaceT {..}) = do
    pure $
      Just
        Domain.Types.TicketPlace.TicketPlace
          { id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            description = description,
            lat = lat,
            lon = lon,
            gallery = gallery,
            shortDesc = shortDesc,
            iconUrl = iconUrl,
            termsAndConditions = termsAndConditions,
            placeType = placeType,
            mapImageUrl = mapImageUrl,
            openTimings = openTimings,
            closeTimings = closeTimings,
            status = status,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  toTType' (Domain.Types.TicketPlace.TicketPlace {..}) = do
    Beam.TicketPlaceT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.description = description,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.gallery = gallery,
        Beam.shortDesc = shortDesc,
        Beam.iconUrl = iconUrl,
        Beam.termsAndConditions = termsAndConditions,
        Beam.placeType = placeType,
        Beam.mapImageUrl = mapImageUrl,
        Beam.openTimings = openTimings,
        Beam.closeTimings = closeTimings,
        Beam.status = status,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
