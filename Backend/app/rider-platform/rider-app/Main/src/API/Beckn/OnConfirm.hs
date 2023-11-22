{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnConfirm (API, handler) where

import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Storage.Queries.Ticket as QRT

type API = OnConfirm.OnConfirmAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReq ->
  FlowHandler AckResponse
onConfirm _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnConfirmReq <- case req.context.domain of
    Context.MOBILITY -> ACL.buildOnConfirmRideReq req
    Context.PUBLIC_TRANSPORT -> ACL.buildOnConfirmBusReq req
    _ -> throwError (InvalidRequest $ "Unsupported Domain: " <> show req.context.domain)

  whenJust mbDOnConfirmReq $ \onConfirmReq ->
    case onConfirmReq.bppBookingId of
      Just bppBookingId ->
        Redis.whenWithLockRedis (onConfirmLockKey bppBookingId.getId) 60 $ do
          validatedReq <- DOnConfirm.validateRequest onConfirmReq
          fork "onConfirm request processing" $
            Redis.whenWithLockRedis (onConfirmProcessingLockKey bppBookingId.getId) 60 $
              DOnConfirm.onConfirm validatedReq
      Nothing -> do
        transactionId <- maybe (throwError $ InternalError "transaction_id is Nothing") return req.context.transaction_id
        ticket <- QRT.findByTransactionId transactionId >>= fromMaybeM (TicketNotFound transactionId)
        void $ QRT.updateBPPTicketId ticket.id (fromJust onConfirmReq.bppTicketId)
        void $ maybe (throwError $ InternalError "bppTicketId is Nothing") (\bppTicketId -> QRT.updateBPPTicketId ticket.id bppTicketId) onConfirmReq.bppTicketId
        case onConfirmReq.bppTicketId of
          Just bppTicketId ->
            Redis.whenWithLockRedis (onConfirmBusLockKey bppTicketId.getId) 60 $ do
              validatedReq <- DOnConfirm.validateRequest onConfirmReq
              fork "onConfirm request processing" $
                Redis.whenWithLockRedis (onConfirmBusProcessingLockKey bppTicketId.getId) 60 $ do
                  DOnConfirm.onConfirm validatedReq
          Nothing -> pure ()
  pure Ack

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "Customer:OnConfirm:BppBookingId-" <> id

onConfirmBusLockKey :: Text -> Text
onConfirmBusLockKey id = "Customer:OnConfirm:BppTicketId-" <> id

onConfirmProcessingLockKey :: Text -> Text
onConfirmProcessingLockKey id = "Customer:OnConfirm:Processing:BppBookingId-" <> id

onConfirmBusProcessingLockKey :: Text -> Text
onConfirmBusProcessingLockKey id = "Customer:OnConfirm:Processing:BppTicketId-" <> id
