{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnUpdate (API, handler) where

import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()

type API = OnUpdate.OnUpdateAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onUpdate

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnUpdateReq <- ACL.buildOnUpdateReq req
  whenJust mbDOnUpdateReq $ \onUpdateReq ->
    Redis.whenWithLockRedis (onUpdateLockKey req.context.message_id) 60 $ do
      validatedOnUpdateReq <- DOnUpdate.validateRequest onUpdateReq
      fork "on update processing" $ do
        Redis.whenWithLockRedis (onUpdateProcessngLockKey req.context.message_id) 60 $
          DOnUpdate.onUpdate validatedOnUpdateReq
  pure Ack

onUpdateLockKey :: Text -> Text
onUpdateLockKey id = "Customer:OnUpdate:MessageId-" <> id

onUpdateProcessngLockKey :: Text -> Text
onUpdateProcessngLockKey id = "Customer:OnUpdate:Processing:MessageId-" <> id
