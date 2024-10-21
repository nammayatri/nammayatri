module ExternalBPP.Bus.ExternalAPI.CUMTA.Order where

import qualified Data.Text as T
import qualified Data.UUID as UU
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import Tools.Error

getOrderId :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CUMTAConfig -> FRFSTicketBooking -> m (Text, Integer)
getOrderId _config booking = do
  case booking.bppOrderId of
    Just bppOrderId -> do
      tickets <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
      ticketNumber :: Integer <-
        listToMaybe tickets <&> (.ticketNumber)
          >>= readMaybe . T.unpack & fromMaybeM (InternalError "Ticket Number Not Found.")
      return (bppOrderId, ticketNumber)
    Nothing -> do
      bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
      let bookingUUIDInt :: Integer = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) -- This should be max 20 characters UUID (Using Transaction UUID)
      return (show bookingUUIDInt, bookingUUIDInt)
