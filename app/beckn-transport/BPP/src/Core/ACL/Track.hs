module Core.ACL.Track where

import Beckn.Product.Validation.Context
import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Track as Track
import Beckn.Types.Id
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Beckn.Utils.Common
import qualified Domain.Action.Beckn.Track as DTrack
import EulerHS.Prelude
import Tools.Error

buildTrackReq ::
  (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) =>
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
