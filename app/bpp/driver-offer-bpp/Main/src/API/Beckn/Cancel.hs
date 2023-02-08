module API.Beckn.Cancel (API, handler) where

import qualified Beckn.Types.Core.Taxi.API.Cancel as API
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import qualified Core.ACL.Cancel as ACL
import qualified Domain.Action.Beckn.Cancel as DCancel
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.CancelAPI

handler :: FlowServer API
handler = cancel

cancel ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Cancel.CancelReq ->
  FlowHandler AckResponse
cancel transporterId subscriber req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Cancel API Flow" "Reached"
    if req.message.item_id /= ""
      then do
        dCancelReq <- ACL.buildCancelSearchReq req
        DCancel.cancelSearch transporterId subscriber dCancelReq
      else do
        dCancelReq <- ACL.buildCancelReq req
        Redis.whenWithLockRedis (cancelLockKey dCancelReq.bookingId.getId) 60 $
          DCancel.cancel transporterId subscriber dCancelReq
    return Ack

cancelLockKey :: Text -> Text
cancelLockKey id = "Driver:Cancel:BookingId-" <> id
