{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Tickets.TicketService where

import qualified Domain.Types.Tickets as DTB
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Tickets.TicketService as BeamTS
import Storage.Queries.Tickets.TicketServicePrice as QTSP

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DTB.TicketService -> m (Maybe DTB.TicketService)
findById (Id id) = findOneWithKV [Se.Is BeamTS.id $ Se.Eq id]

getTicketServicesByPlaceId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DTB.TicketPlace -> m [DTB.TicketService]
getTicketServicesByPlaceId (Id placeId) = findAllWithKV [Se.Is BeamTS.placesId $ Se.Eq placeId]

instance FromTType' BeamTS.TicketService DTB.TicketService where
  fromTType' BeamTS.TicketServiceT {..} = do
    prices <- QTSP.findByTicketServiceId id
    pure $
      Just $
        DTB.TicketService
          { id = Id id,
            ..
          }

instance ToTType' BeamTS.TicketService DTB.TicketService where
  toTType' DTB.TicketService {..} =
    BeamTS.TicketServiceT
      { BeamTS.id = getId id,
        BeamTS.placesId = placesId,
        BeamTS.service = service,
        BeamTS.maxVerification = maxVerification,
        BeamTS.openTimings = openTimings,
        BeamTS.closeTimings = closeTimings,
        BeamTS.validityTimings = validityTimings
      }
