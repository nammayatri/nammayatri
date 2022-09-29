module Core.ACL.OnTrack (buildOnTrackReq) where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import Beckn.Storage.Hedis as Redis
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.OnTrack as OnTrack
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
import Types.Error
import Utils.Common

buildOnTrackReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    HedisFlow m r
  ) =>
  OnTrack.OnTrackReq ->
  m (Maybe DOnTrack.OnTrackReq)
buildOnTrackReq req = do
  validateContext Context.ON_TRACK req.context
  handleError req.contents $ \message -> do
    bppRideId <- Redis.get key >>= fromMaybeM (InternalError "Track:bppRideId not found.")
    Redis.del key
    return $
      DOnTrack.OnTrackReq
        { trackUrl = message.tracking.url,
          ..
        }
  where
    key = "Track:bppRideId:" <> req.context.message_id

handleError ::
  (MonadFlow m) =>
  Either Error OnTrack.OnTrackMessage ->
  (OnTrack.OnTrackMessage -> m DOnTrack.OnTrackReq) ->
  m (Maybe DOnTrack.OnTrackReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_track req" $ "on_track error: " <> show err
      pure Nothing
