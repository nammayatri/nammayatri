{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | At a run's check-in time (SharedRun.Knobs.checkInDue), ask the driver holding it to acknowledge, unless they have.
-- Scheduled once per run by SharedLogic.SharedRun.Coverage.scheduleCoverageJobs; a no-op on a run that is gone,
-- unassigned or acknowledged, so a late or repeated run is harmless.
module SharedLogic.Allocator.Jobs.SharedRun.SharedRunCheckIn
  ( sharedRunCheckIn,
  )
where

import Control.Applicative ((<|>))
import qualified Domain.Types.Booking as Fields
import qualified Domain.Types.Location as Fields
import qualified Domain.Types.Ride as Fields
import qualified Domain.Types.TransporterConfig as Fields
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified SharedLogic.SharedRun.Coverage as Coverage
import SharedLogic.SharedRun.Coverage.Rules (CheckInDecision (..), decideCheckIn)
import qualified Storage.Queries.Booking as QRB
import qualified Tools.Notifications as Notify

sharedRunCheckIn :: (Coverage.CoverageFlow m r, Hedis.HedisLTSFlowEnv r) => Job 'SharedRunCheckIn -> m ExecutionResult
sharedRunCheckIn Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let bookingId = jobInfo.jobData.bookingId
  QRB.findById bookingId >>= \case
    Nothing -> pure (Terminate $ "shared run " <> bookingId.getId <> " not found")
    Just booking ->
      Coverage.runSnapshot booking >>= \case
        Nothing -> pure (Terminate "the run's city has no shared-run supply knobs")
        Just snapshot -> do
          case (decideCheckIn snapshot.state, snapshot.ride) of
            (AskToAcknowledge, Just ride) -> do
              Notify.notifySharedRunAckRequest booking.merchantOperatingCityId ride.driverId snapshot.transporterConfig.timeDiffFromUtc (ackRequestOf booking ride.id)
              logInfo $ "shared run " <> booking.id.getId <> ": asked driver " <> ride.driverId.getId <> " to acknowledge"
            (decision, _) -> logInfo $ "shared run " <> booking.id.getId <> ": no check-in request: " <> show decision
          pure Complete
  where
    ackRequestOf booking rideId =
      Notify.SharedRunAckRequestEntityData
        { rideId = rideId.getId,
          bookingId = booking.id.getId,
          departure = booking.startTime,
          routeCode = fromMaybe "" booking.routeCode,
          firstStopName = fromMaybe "" (booking.fromLocation.address.area <|> booking.fromLocation.address.fullAddress)
        }
