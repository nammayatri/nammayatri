{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Status
  ( buildStatusReqV2,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.UUID as UUID
import qualified Domain.Types.Beckn.Status as DStatus
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildStatusReqV2 ::
  (HasFlowEnv m r '["_version" ::: Text]) =>
  Subscriber.Subscriber ->
  Spec.StatusReq ->
  m DStatus.DStatusReq
buildStatusReqV2 subscriber req = do
  ContextV2.validateContext Context.STATUS req.statusReqContext
  unless (Just subscriber.subscriber_id == req.statusReqContext.contextBapId) $
    throwError (InvalidRequest "Invalid bap_id")

  statusReqMessageRefId <- req.statusReqMessage.statusReqMessageRefId & fromMaybeM (InvalidRequest "Invalid statusReqMessageRefId")
  transactionId <- (fmap UUID.toText req.statusReqContext.contextTransactionId) & fromMaybeM (InvalidRequest "TransactionId not found")
  let bookingId = Just $ Id statusReqMessageRefId
  return $
    DStatus.StatusReq
      { ..
      }
