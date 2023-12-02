{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketPlace where

import qualified Domain.Types.Merchant.MerchantOperatingCity as Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketPlace as Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketPlace as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.TicketPlace.TicketPlace -> m ()
create = createWithKV

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
