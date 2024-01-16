{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnTrack (buildOnTrackReq) where

import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Beckn.Types.Core.Taxi.OnTrack as OnTrack
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
import Kernel.Prelude
import Kernel.Product.Validation.Context
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import Tools.Error

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

_buildOnTrackReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    HedisFlow m r
  ) =>
  Spec.OnTrackReq ->
  m (Maybe DOnTrack.OnTrackReq)
_buildOnTrackReqV2 req = do
  ContextV2.validateContext Context.ON_TRACK req.onTrackReqContext
  _handleErrorV2 req $ \_message -> do
    messageUuid <- req.onTrackReqContext.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
    let messageId = UUID.toText messageUuid
    bppRideId <- Redis.get (key messageId) >>= fromMaybeM (InternalError "Track:bppRideId not found.")
    Redis.del $ key messageId
    case parsedData of
      Left err -> do
        logTagError "on_track req" $ "on_track error: " <> show err
        pure Nothing
      Right (trackUrl) ->
        return $
          Just $
            DOnTrack.OnTrackReq
              { trackUrl = trackUrl,
                ..
              }
  where
    key messageId = "Track:bppRideId:" <> messageId
    parsedData :: Either Text (BaseUrl)
    parsedData = do
      message <- req.onTrackReqMessage & maybe (Left "Missing on_track message") Right

      trackUrl <-
        message.onTrackReqMessageTracking.trackingUrl
          >>= parseBaseUrl
          & maybe (Left "Missing tracking url") Right

      Right trackUrl

_handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnTrackReq ->
  (Spec.OnTrackReqMessage -> m (Maybe DOnTrack.OnTrackReq)) ->
  m (Maybe DOnTrack.OnTrackReq)
_handleErrorV2 req action = do
  case req.onTrackReqError of
    Nothing -> req.onTrackReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_track req" $ "on_track error: " <> show err
      pure Nothing
