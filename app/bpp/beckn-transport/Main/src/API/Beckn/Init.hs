module API.Beckn.Init (API, handler) where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as API
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnInit as ACL
import Core.Beckn (withCallback')
import Domain.Action.Beckn.Cancel
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Action.Beckn.Init as DInit
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import Environment
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> API.InitAPI

handler :: FlowServer API
handler = initImpl

initImpl ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Init.InitReq ->
  FlowHandler AckResponse
initImpl transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    dInitReq <- ACL.buildInitReq subscriber req
    Redis.whenWithLockRedis (initLockKey dInitReq.transactionId) 60 $ do
      let context = req.context
      dInitRes <- DInit.init transporterId dInitReq
      let cancelReq = makeCancelReq dInitRes.booking.id
      void . handle (errHandler cancelReq) $
        withCallback' withShortRetry dInitRes.transporter Context.INIT OnInit.onInitAPI context context.bap_uri $
          -- there should be DOnInit.onInit, but it is empty anyway
          pure $ ACL.mkOnInitMessage dInitRes
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

initLockKey :: Text -> Text
initLockKey id = "Driver:Init:TransactionId-" <> id
