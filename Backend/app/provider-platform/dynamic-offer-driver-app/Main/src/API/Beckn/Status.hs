{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Status (API, handler) where

import qualified Beckn.ACL.OnStatus as ACL
import qualified Beckn.ACL.Status as ACL
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.API.Status as Status
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.Status as DStatus
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Tools.Logging
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Status.StatusAPIV2

handler :: FlowServer API
handler = status

status ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Status.StatusReqV2 ->
  FlowHandler AckResponse
status transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI $
  withDynamicLogLevel "bpp-status-api" $ do
    txnId <- Utils.getTransactionId reqV2.statusReqContext
    Utils.withTransactionIdLogTag txnId $ do
      logDebug $ "BPP_STATUS_API_DEBUG: Received status request for transactionId: " <> txnId
      logTagInfo "Status APIV2 Flow" $ "Reached:-" <> show reqV2
      dStatusReq <- ACL.buildStatusReqV2 subscriber reqV2
      let context = reqV2.statusReqContext
      callbackUrl <- Utils.getContextBapUri context
      logDebug $ "BPP_STATUS_API_DEBUG: Built status request, callback URL: " <> show callbackUrl
      dStatusRes <- DStatus.handler transporterId dStatusReq
      logDebug $ "BPP_STATUS_API_DEBUG: Status handler completed for booking: " <> dStatusRes.booking.id.getId
      fork "status received pushing ondc logs" do
        void $ pushLogs "status" (toJSON reqV2) dStatusRes.booking.providerId.getId "MOBILITY"
      internalEndPointHashMap <- asks (.internalEndPointHashMap)
      msgId <- Utils.getMessageId context
      onStatusReq <- ACL.buildOnStatusReqV2 dStatusRes.transporter dStatusRes.booking dStatusRes.info (Just msgId)
      logDebug $ "BPP_STATUS_API_DEBUG: Built on_status request with messageId: " <> msgId <> " for booking: " <> dStatusRes.booking.id.getId
      Callback.withCallback dStatusRes.transporter "on_status" OnStatus.onStatusAPIV2 callbackUrl internalEndPointHashMap (errHandler onStatusReq.onStatusReqContext) $
        pure onStatusReq

errHandler :: Spec.Context -> BecknAPIError -> Spec.OnStatusReq
errHandler context (BecknAPIError err) =
  Spec.OnStatusReq
    { onStatusReqContext = context,
      onStatusReqError = Just err',
      onStatusReqMessage = Nothing
    }
  where
    err' =
      Spec.Error
        { errorCode = Just err.code,
          errorMessage = err.message >>= \m -> Just $ encodeToText err._type <> " " <> m,
          errorPaths = err.path
        }
