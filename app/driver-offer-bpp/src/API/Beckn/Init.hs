module API.Beckn.Init (API, handler) where

import Beckn.Prelude hiding (init)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnInit as ACL
import qualified Core.Beckn as CallBAP
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Merchant as DM
import Environment
import Servant hiding (throwError)

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Init.InitAPI

handler :: FlowServer API
handler = init

init ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Init.InitReq ->
  FlowHandler AckResponse
init transporterId (SignatureAuthResult _ subscriber _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Init API Flow" "Reached"
    dInitReq <- ACL.buildInitReq subscriber req
    Redis.whenWithLockRedis (initLockKey dInitReq.driverQuoteId.getId) 60 $ do
      let context = req.context
      dInitRes <- DInit.handler transporterId dInitReq
      void . handle (errHandler dInitRes.booking) $
        CallBAP.withCallback dInitRes.transporter Context.INIT OnInit.onInitAPI context context.bap_uri $
          pure $ ACL.mkOnInitMessage dInitRes
      return ()
    pure Ack
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DInit.cancelBooking booking transporterId
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DInit.cancelBooking booking transporterId
      | otherwise = throwM exc

initLockKey :: Text -> Text
initLockKey id = "Driver:Init:DriverQuoteId-" <> id
