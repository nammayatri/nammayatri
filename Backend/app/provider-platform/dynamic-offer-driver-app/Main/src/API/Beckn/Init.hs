{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Init (API, handler) where

import qualified Beckn.ACL.Init as ACL
import qualified Beckn.ACL.OnInit as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude hiding (init)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
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
init transporterId (SignatureAuthResult _ subscriber) req =
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
