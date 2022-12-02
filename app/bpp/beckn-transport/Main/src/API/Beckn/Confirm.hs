module API.Beckn.Confirm (API, handler) where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnConfirm as ACL
import Core.Beckn (withCallback)
import Domain.Action.Beckn.Cancel
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import Environment
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.ConfirmAPI

handler :: FlowServer API
handler = confirm

confirm ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "confirm API Flow" "Reached"
    dConfirmReq <- ACL.buildConfirmReq subscriber req
    Redis.whenWithLockRedis (confirmLockKey dConfirmReq.bookingId.getId) 60 $ do
      let context = req.context
      dConfirmRes <- DConfirm.confirm transporterId subscriber dConfirmReq
      let cancelReq = makeCancelReq dConfirmRes.booking.id
      void . handle (errHandler cancelReq) $
        withCallback dConfirmRes.transporter Context.CONFIRM OnConfirm.onConfirmAPI context context.bap_uri $
          -- there should be DOnConfirm.onConfirm, but it is empty anyway
          pure $ ACL.mkOnConfirmMessage dConfirmRes
      return ()
    pure Ack
  where
    errHandler cancelReq exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancel transporterId cancelReq
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancel transporterId cancelReq
      | otherwise = throwM exc

    makeCancelReq bookingId =
      CancelReq
        { bookingId = bookingId
        }

confirmLockKey :: Text -> Text
confirmLockKey id = "Driver:Confirm:BookingId-" <> id
