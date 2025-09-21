module ExternalBPP.ExternalAPI.Direct.Verify where

import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Utils (mapConcurrently)
import ExternalBPP.ExternalAPI.Direct.Utils
import ExternalBPP.ExternalAPI.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSTicket as QFRFSTicket

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DIRECTConfig -> Text -> m TicketPayload
verifyTicket config encryptedQrData = decodeQR config encryptedQrData

generateUpdatedQRTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DIRECTConfig -> Id FRFSTicketBooking -> (TicketPayload -> m TicketPayload) -> m [TicketPayload]
generateUpdatedQRTicket config bookingId updateTicketFn = do
  tickets <- QFRFSTicket.findAllByTicketBookingId bookingId
  decodedTicketPayload <- mapConcurrently (\ticket -> decodeQR config ticket.qrData) tickets
  newTicketPayloads <- mapConcurrently updateTicketFn decodedTicketPayload
  return newTicketPayloads
