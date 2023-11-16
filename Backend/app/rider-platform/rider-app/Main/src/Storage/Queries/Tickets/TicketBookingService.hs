{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Tickets.TicketBookingService where

import qualified Domain.Types.Tickets as DomainTB
import qualified Domain.Types.Tickets.TicketBooking as DomainTB
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Tickets.TicketBookingService as BeamTBS
import qualified Storage.Queries.Tickets.TicketBookingServicePriceBreakup as QTBSPB

createMany :: MonadFlow m => [DomainTB.TicketBookingService] -> m ()
createMany ticketBookingServices = do
  mapM_ (QTBSPB.createMany . (.prices)) ticketBookingServices
  mapM_ createWithKV ticketBookingServices

findAllByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DomainTB.TicketBooking -> m [DomainTB.TicketBookingService]
findAllByBookingId ticketBookingId = findAllWithKV [Se.Is BeamTBS.ticketBookingId $ Se.Eq ticketBookingId.getId]

findByShortId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => ShortId DomainTB.TicketBookingService -> m (Maybe DomainTB.TicketBookingService)
findByShortId (ShortId shortId) = findOneWithKV [Se.Is BeamTBS.shortId $ Se.Eq shortId]

updateVerification :: MonadFlow m => Id DomainTB.TicketBookingService -> Int -> UTCTime -> m ()
updateVerification (Id id) verfCount now =
  updateWithKV
    [ Se.Set BeamTBS.status DomainTB.Verified,
      Se.Set BeamTBS.verificationCount verfCount,
      Se.Set BeamTBS.updatedAt now
    ]
    [Se.Is BeamTBS.id $ Se.Eq id]

updateAllStatusByBookingId :: MonadFlow m => Id DomainTB.TicketBooking -> DomainTB.ServiceStatus -> m ()
updateAllStatusByBookingId bookingId status = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamTBS.status status,
      Se.Set BeamTBS.updatedAt now
    ]
    [Se.Is BeamTBS.ticketBookingId $ Se.Eq bookingId.getId]

instance FromTType' BeamTBS.TicketBookingService DomainTB.TicketBookingService where
  fromTType' BeamTBS.TicketBookingServiceT {..} = do
    prices <- QTBSPB.findByTicketBookingServiceId id
    pure $
      Just $
        DomainTB.TicketBookingService
          { id = Id id,
            shortId = ShortId shortId,
            ticketBookingId = Id ticketBookingId,
            ticketServiceId = Id ticketServiceId,
            status = status,
            merchantOperatingCityId = Id merchantOperatingCityId,
            prices,
            ..
          }

instance ToTType' BeamTBS.TicketBookingService DomainTB.TicketBookingService where
  toTType' DomainTB.TicketBookingService {..} =
    BeamTBS.TicketBookingServiceT
      { BeamTBS.id = getId id,
        BeamTBS.shortId = getShortId shortId,
        BeamTBS.ticketBookingId = getId ticketBookingId,
        BeamTBS.ticketServiceId = getId ticketServiceId,
        BeamTBS.amount = amount,
        BeamTBS.status = status,
        BeamTBS.verificationCount = verificationCount,
        BeamTBS.expiryDate = expiryDate,
        BeamTBS.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamTBS.createdAt = createdAt,
        BeamTBS.updatedAt = updatedAt
      }
