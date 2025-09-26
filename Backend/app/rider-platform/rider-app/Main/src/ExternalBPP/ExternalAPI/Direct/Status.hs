module ExternalBPP.ExternalAPI.Direct.Status where

import Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus as Ticket
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Direct.Utils
import ExternalBPP.ExternalAPI.Types
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSTicket as QFRFSTicket

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DIRECTConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus config booking = do
  tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  updatedTickets <-
    mapM
      ( \ticket -> do
          if ticket.status == Ticket.ACTIVE
            then do
              refreshQrData <- refreshQR config ticket.qrData
              (qrData, qrRefreshAt) <-
                case refreshQrData of
                  Just (qrData', qrRefreshAt') -> pure (qrData', Just qrRefreshAt')
                  Nothing -> pure (ticket.qrData, ticket.qrRefreshAt)
              return $
                Just $
                  ProviderTicket
                    { ticketNumber = ticket.ticketNumber,
                      vehicleNumber = Nothing,
                      qrData,
                      qrValidity = ticket.validTill,
                      description = ticket.description,
                      qrStatus = "UNCLAIMED",
                      qrRefreshAt,
                      commencingHours = Nothing
                    }
            else pure Nothing
      )
      tickets
  return $ catMaybes updatedTickets
