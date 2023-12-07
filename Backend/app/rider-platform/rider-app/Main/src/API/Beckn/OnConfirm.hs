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
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = OnConfirm.OnConfirmAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReqV2 ->
  FlowHandler AckResponse
onConfirm _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnConfirmReq <- ACL.buildOnConfirmReq req
  whenJust mbDOnConfirmReq $ \onConfirmReq ->
    Redis.whenWithLockRedis (onConfirmLockKey onConfirmReq.bppBookingId.getId) 60 $ do
      validatedReq <- DOnConfirm.validateRequest onConfirmReq
      fork "onConfirm request processing" $
        Redis.whenWithLockRedis (onConfirmProcessingLockKey onConfirmReq.bppBookingId.getId) 60 $
          DOnConfirm.onConfirm validatedReq
  pure Ack

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "Customer:OnConfirm:BppBookingId-" <> id

onConfirmProcessingLockKey :: Text -> Text
onConfirmProcessingLockKey id = "Customer:OnConfirm:Processing:BppBookingId-" <> id
