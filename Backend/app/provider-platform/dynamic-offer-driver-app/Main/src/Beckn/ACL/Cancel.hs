{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Cancel where

import Beckn.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Data.Maybe (fromJust)
import qualified Domain.Action.Beckn.Cancel as DCancel
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

buildCancelReqV2 ::
  (HasFlowEnv m r '["_version" ::: Text]) =>
  Spec.CancelReq ->
  m DCancel.CancelReq
buildCancelReqV2 req = do
  ContextV2.validateContext Context.CANCEL req.cancelReqContext
  if isJust (req.cancelReqMessage.cancelReqMessageDescriptor)
    then do
      let bookingId = Id $ req.cancelReqMessage.cancelReqMessageOrderId
      let descriptor = fromJust $ req.cancelReqMessage.cancelReqMessageDescriptor
      let userReallocationEnabled = req.cancelReqMessage.cancelReqMessageReallocate
      let cancellationReason = getCancellationReason req
      return $
        DCancel.CancelRide $
          DCancel.CancelRideReq
            { cancelStatus = descriptor.descriptorCode,
              ..
            }
    else do
      let transactionId = req.cancelReqMessage.cancelReqMessageOrderId
      return $
        DCancel.CancelSearch $
          DCancel.CancelSearchReq
            { ..
            }
