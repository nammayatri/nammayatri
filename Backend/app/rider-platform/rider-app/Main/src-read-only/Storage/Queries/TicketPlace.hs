{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketPlace where

import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketPlace as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketPlace.TicketPlace -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.TicketPlace.TicketPlace] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe (Domain.Types.TicketPlace.TicketPlace))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

getTicketPlaces :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findAllWithKV
    [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe (Domain.Types.TicketPlace.TicketPlace))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> [Kernel.Prelude.Text] -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay -> Domain.Types.TicketPlace.PlaceType -> Kernel.Prelude.Text -> [Kernel.Prelude.Text] -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m ()
updateByPrimaryKey closeTimings description gallery iconUrl lat lon mapImageUrl (Kernel.Types.Id.Id merchantOperatingCityId) name openTimings placeType shortDesc termsAndConditions (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.closeTimings closeTimings,
      Se.Set Beam.description description,
      Se.Set Beam.gallery gallery,
      Se.Set Beam.iconUrl iconUrl,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.mapImageUrl mapImageUrl,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.name name,
      Se.Set Beam.openTimings openTimings,
      Se.Set Beam.placeType placeType,
      Se.Set Beam.shortDesc shortDesc,
      Se.Set Beam.termsAndConditions termsAndConditions
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

instance FromTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  fromTType' Beam.TicketPlaceT {..} = do
    pure $
      Just
        Domain.Types.TicketPlace.TicketPlace
          { closeTimings = closeTimings,
            description = description,
            gallery = gallery,
            iconUrl = iconUrl,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            mapImageUrl = mapImageUrl,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            openTimings = openTimings,
            placeType = placeType,
            shortDesc = shortDesc,
            termsAndConditions = termsAndConditions
          }

instance ToTType' Beam.TicketPlace Domain.Types.TicketPlace.TicketPlace where
  toTType' Domain.Types.TicketPlace.TicketPlace {..} = do
    Beam.TicketPlaceT
      { Beam.closeTimings = closeTimings,
        Beam.description = description,
        Beam.gallery = gallery,
        Beam.iconUrl = iconUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.mapImageUrl = mapImageUrl,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.openTimings = openTimings,
        Beam.placeType = placeType,
        Beam.shortDesc = shortDesc,
        Beam.termsAndConditions = termsAndConditions
      }
