module API.Beckn.OnConfirm.Handler where

import qualified API.Beckn.OnConfirm.Types as OnConfirm
import App.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Core.API.Types (BecknCallbackReq)
import qualified Core.Context as Context
import qualified Core.OnConfirm as OnConfirm
import qualified Domain.Booking as DBooking
import qualified Storage.Queries.Booking as QBooking
import Tools.Context (validateContext)
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnConfirm.API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  BecknCallbackReq OnConfirm.OnConfirmMessage ->
  FlowHandler AckResponse
onConfirm _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_CONFIRM $ req.context
  case req.contents of
    Right msg -> handleOnConfirm (Id req.context.transaction_id) msg
    Left err -> logTagError "on_search req" $ "on_search error: " <> showT err
  return Ack

handleOnConfirm :: EsqDBFlow m r => Id DBooking.Booking -> OnConfirm.OnConfirmMessage -> m ()
handleOnConfirm bookingId msg = do
  booking <- QBooking.findById bookingId >>= fromMaybeM BookingDoesNotExist
  ticket <- listToMaybe msg.order.items & fromMaybeM (InvalidRequest "Empty items field.")
  let updBooking =
        booking{status = DBooking.AWAITING_PAYMENT,
                ticketId = Just ticket.id,
                ticketCreatedAt = Just msg.order.created_at
               }
  runTransaction $ QBooking.update updBooking