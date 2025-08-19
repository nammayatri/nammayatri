{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.FRFS.OnCancel where

import qualified Beckn.ACL.FRFS.OnCancel as ACL
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFS.OnCancel as DOnCancel
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.FRFSUtils as FRFSUtils
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import TransactionLogs.PushLogs

type API = Spec.OnCancelAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onCancel

onCancel ::
  SignatureAuthResult ->
  Spec.OnCancelReq ->
  FlowHandler Spec.AckResponse
onCancel _ req = withFlowHandlerAPI $ do
  transaction_id <- req.onCancelReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  message_id <- req.onCancelReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  withTransactionIdLogTag' transaction_id $ do
    ticketBooking <- runInReplica $ QTBooking.findBySearchId (Id transaction_id) >>= fromMaybeM (BookingDoesNotExist message_id)
    logDebug $ "Received OnCancel request" <> encodeToText req
    case req.onCancelReqError of
      Just err -> whenJust err.errorCode $ \errorCode -> do
        when (errorCode == "50001") $ QTBooking.updateIsBookingCancellableByBookingId (Just False) ticketBooking.id -- TODO: Add Error Code Properly
      Nothing -> do
        dOnCancelReq <- ACL.buildOnCancelReq req
        if isJust dOnCancelReq
          then do
            let onCancelReq = fromJust dOnCancelReq
            Redis.whenWithLockRedis (onCancelLockKey onCancelReq.bppOrderId) 60 $ do
              (merchant, booking) <- DOnCancel.validateRequest onCancelReq
              fork "onCancel request processing" $
                Redis.whenWithLockRedis (onCancelProcessingLockKey onCancelReq.bppOrderId) 60 $
                  DOnCancel.onCancel merchant booking onCancelReq
              fork "FRFS onCancel received pushing ondc logs" do
                void $ pushLogs "on_cancel" (toJSON req) merchant.id.getId "PUBLIC_TRANSPORT"
          else do
            void $ Redis.del (FRFSUtils.makecancelledTtlKey ticketBooking.id)
    pure Utils.ack

onCancelLockKey :: Text -> Text
onCancelLockKey id = "FRFS:OnCancel:bppOrderId-" <> id

onCancelProcessingLockKey :: Text -> Text
onCancelProcessingLockKey id = "FRFS:OnCancel:Processing:bppOrderId-" <> id
