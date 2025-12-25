{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnTrack
  ( buildOnTrackReqV2,
  )
where

import qualified Beckn.OnDemand.Utils.Common as Common
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import Tools.Error

buildOnTrackReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    HedisFlow m r
  ) =>
  Spec.OnTrackReq ->
  m (Maybe DOnTrack.OnTrackReq)
buildOnTrackReqV2 req = do
  ContextV2.validateContext Context.ON_TRACK req.onTrackReqContext
  handleErrorV2 req $ \message -> do
    messageUuid <- req.onTrackReqContext.contextMessageId & fromMaybeM (InvalidBecknSchema "Missing message_id in on_track req")
    let messageId = UUID.toText messageUuid
    bppRideId <- Redis.get (key messageId) >>= fromMaybeM (InternalError "Track:bppRideId not found.")
    Redis.del $ key messageId
    let trackUrl = message.onTrackReqMessageTracking.trackingUrl >>= parseBaseUrl
    trackingLocation <- case message.onTrackReqMessageTracking.trackingLocation of
      Nothing -> return Nothing
      Just loc -> do
        trackingLocationGPS <- loc.locationGps & fromMaybeM (InvalidBecknSchema "Missing Tracking Location GPS in on_track req")
        parsedGPS <- Common.parseGPS trackingLocationGPS
        trackingLocationUpdatedAt <- loc.locationUpdatedAt & fromMaybeM (InvalidBecknSchema "Missing Tracking Location UpdatedAt in on_track req")
        return $
          Just $
            DOnTrack.TrackingLocation
              { gps = parsedGPS,
                updatedAt = trackingLocationUpdatedAt
              }
    return $
      Just $
        DOnTrack.OnTrackReq
          { trackUrl = trackUrl,
            ..
          }
  where
    key messageId = "Track:bppRideId:" <> messageId

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnTrackReq ->
  (Spec.OnTrackReqMessage -> m (Maybe DOnTrack.OnTrackReq)) ->
  m (Maybe DOnTrack.OnTrackReq)
handleErrorV2 req action = do
  case req.onTrackReqError of
    Nothing -> req.onTrackReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_track req" $ "on_track error: " <> show err
      pure Nothing
