{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Tickets.TicketPlace where

import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Tickets.TicketPlace as DomainTP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Tickets.TicketPlace as BeamTP

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id TicketPlace -> m (Maybe TicketPlace)
findById (Id id) = findOneWithKV [Se.Is BeamTP.id $ Se.Eq id]

getTicketPlaces :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [TicketPlace]
getTicketPlaces (Id merchantOperatingCityId) = findAllWithKV [Se.Is BeamTP.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

instance FromTType' BeamTP.TicketPlace TicketPlace where
  fromTType' BeamTP.TicketPlaceT {..} =
    pure $
      Just $
        TicketPlace
          { id = Id id,
            merchantOperatingCityId = Id merchantOperatingCityId,
            ..
          }

instance ToTType' BeamTP.TicketPlace TicketPlace where
  toTType' TicketPlace {..} =
    BeamTP.TicketPlaceT
      { BeamTP.id = getId id,
        BeamTP.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamTP.name = name,
        BeamTP.description = description,
        BeamTP.lat = lat,
        BeamTP.lon = lon,
        BeamTP.gallery = gallery,
        BeamTP.shortDesc = shortDesc,
        BeamTP.iconUrl = iconUrl,
        BeamTP.mapImageUrl = mapImageUrl,
        BeamTP.openTimings = openTimings,
        BeamTP.closeTimings = closeTimings
      }
