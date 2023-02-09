module API.Beckn.Confirm (API, handler) where

import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnConfirm as ACL
import Beckn.Core (withCallback')
import qualified Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Domain.Action.Beckn.Cancel
import qualified Domain.Action.Beckn.Confirm as DConfirm
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
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
        withCallback' withShortRetry dConfirmRes.transporter Context.CONFIRM OnConfirm.onConfirmAPI context context.bap_uri $
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
