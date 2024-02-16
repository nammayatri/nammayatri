{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Track where

import qualified Beckn.Types.Core.Taxi.API.Track as Track
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.Track as DTrack
import EulerHS.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildTrackReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Track.TrackReq ->
  m DTrack.DTrackReq
buildTrackReq subscriber req = do
  validateContext Context.TRACK req.context
  unless (subscriber.subscriber_id == req.context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  let bookingId = Id req.message.order_id
  return $
    DTrack.TrackReq
      { ..
      }

buildTrackReqV2 ::
  (HasFlowEnv m r '["_version" ::: Text]) =>
  Subscriber.Subscriber ->
  Spec.TrackReq ->
  m DTrack.DTrackReq
buildTrackReqV2 subscriber req = do
  ContextV2.validateContext Context.TRACK req.trackReqContext
  bap_id <- req.trackReqContext.contextBapId & fromMaybeM (InvalidRequest "Missing bap_id")
  unless (subscriber.subscriber_id == bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  let bookingId = Id req.trackReqMessage.trackReqMessageOrderId
  return $
    DTrack.TrackReq
      { ..
      }
