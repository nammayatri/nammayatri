module ExternalBPP.Bus.ExternalAPI.CUMTA.Status where

import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.Bus.ExternalAPI.Types
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSTicket as QFRFSTicket

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CUMTAConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus _config booking = do
  tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  return $
    map
      ( \ticket -> do
          ProviderTicket
            { ticketNumber = ticket.ticketNumber,
              qrData = ticket.qrData,
              qrStatus = "UNCLAIMED",
              qrValidity = ticket.validTill
            }
      )
      tickets
