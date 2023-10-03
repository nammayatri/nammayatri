module Domain.Action.UI.ConfirmBus where

import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ticket as DTT
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.Queries.Ticket as QRideT

confirmBus ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  Id DP.Person ->
  Id DQuote.Quote ->
  Maybe Integer ->
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  m SConfirm.DConfirmBusRes
confirmBus personId quoteId quantity paymentMethodId = SConfirm.confirmBus SConfirm.DConfirmReq {..}

-- cancel ticket generation when QUOTE_EXPIRED on bpp side, or other EXTERNAL_API_CALL_ERROR catched
cancelTicket :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r) => DTT.Ticket -> m ()
cancelTicket ticket = do
  _ <- QRideT.updateStatus ticket.id DTT.CANCELLED
  logTagInfo ("TicketId-" <> getId ticket.id) ("Cancellation reason " <> show DBCR.ByApplication)
