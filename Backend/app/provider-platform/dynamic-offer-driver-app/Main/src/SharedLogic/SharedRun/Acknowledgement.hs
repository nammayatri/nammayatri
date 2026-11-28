{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The assigned driver's acknowledgement of a shared-route run: owned-fleet drivers are assigned without being asked,
-- so before departure they confirm they will drive it, and the fallback job unassigns a run nobody confirmed.
--
-- Keyed by ride, so a re-assignment (a new ride) starts unacknowledged. Kept in Redis until a day after the run's
-- departure; the storage stays behind these functions, so ops and the control centre read it without knowing the key.
module SharedLogic.SharedRun.Acknowledgement
  ( acknowledgeRun,
    isRunAcknowledged,
    clearAcknowledgement,
  )
where

import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

ackKey :: Id DRide.Ride -> Text
ackKey rideId = "sharedRun:ack:" <> rideId.getId

-- | Idempotent: acknowledging twice keeps the first.
acknowledgeRun :: (CacheFlow m r, MonadFlow m) => Id DRide.Ride -> UTCTime -> m ()
acknowledgeRun rideId departure = do
  now <- getCurrentTime
  let ttl = max 60 (ceiling (diffUTCTime (addUTCTime 86400 departure) now))
  void $ Hedis.withMasterRedis $ Hedis.setNxExpire (ackKey rideId) ttl True

isRunAcknowledged :: (CacheFlow m r, MonadFlow m) => Id DRide.Ride -> m Bool
isRunAcknowledged rideId = fromMaybe False <$> Hedis.withMasterRedis (Hedis.get (ackKey rideId))

clearAcknowledgement :: (CacheFlow m r, MonadFlow m) => Id DRide.Ride -> m ()
clearAcknowledgement rideId = void $ Hedis.withMasterRedis $ Hedis.del (ackKey rideId)
