{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Status where

import qualified Beckn.Types.Core.Taxi.API.Status as Status
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.Status as DStatus
import EulerHS.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildStatusReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Status.StatusReq ->
  m DStatus.DStatusReq
buildStatusReq subscriber req = do
  validateContext Context.STATUS req.context
  unless (subscriber.subscriber_id == req.context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")

  let bookingId = Id req.message.order_id
  return $
    DStatus.StatusReq
      { ..
      }

_buildStatusReq ::
  (HasFlowEnv m r '["_version" ::: Text]) =>
  Subscriber.Subscriber ->
  Spec.StatusReq ->
  m DStatus.DStatusReq
_buildStatusReq subscriber req = do
  ContextV2.validateContext Context.STATUS req.statusReqContext
  unless (Just subscriber.subscriber_id == req.statusReqContext.contextBapId) $
    throwError (InvalidRequest "Invalid bap_id")

  statusReqMessageRefId <- req.statusReqMessage.statusReqMessageRefId & fromMaybeM (InvalidRequest "Invalid statusReqMessageRefId")
  let bookingId = Id statusReqMessageRefId
  return $
    DStatus.StatusReq
      { ..
      }
