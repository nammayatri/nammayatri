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
import Beckn.Core (withCallback')
import qualified Beckn.Types.Core.Taxi.API.Init as API
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Domain.Action.Beckn.Cancel
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Action.Beckn.Init as DInit
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
    :> API.InitAPI

handler :: FlowServer API
handler = initImpl

initImpl ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Init.InitReq ->
  FlowHandler AckResponse
initImpl transporterId (SignatureAuthResult _ subscriber) req =
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
