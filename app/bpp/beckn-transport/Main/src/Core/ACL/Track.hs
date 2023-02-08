module Core.ACL.Track where

import qualified Beckn.Types.Core.Taxi.API.Track as Track
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
  let rideId = Id req.message.order.id
  return $
    DTrack.TrackReq
      { ..
      }
