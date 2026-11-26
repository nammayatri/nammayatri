{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The effects behind keeping a shared-route run covered (the decisions are "SharedLogic.SharedRun.Coverage.Rules"):
-- scheduling the run's check-in and fallback jobs, reading the run's state for them, building the owned-fleet
-- candidates and telling ops once that a run is not covered.
module SharedLogic.SharedRun.Coverage
  ( CoverageFlow,
    RunSnapshot (..),
    scheduleCoverageJobs,
    runSnapshot,
    ownedFleetCandidates,
    alertOpsOnce,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types as DTrip
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.SharedRoute (SharedSupplyKnobs)
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleRouteMapping as Fields
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), SchedulerJobFlow, SharedRunCheckInJobData (..), SharedRunFallbackJobData (..))
import qualified SharedLogic.ScheduledBooking.OpsNote as OpsNote
import qualified SharedLogic.SharedRun.Acknowledgement as Ack
import qualified SharedLogic.SharedRun.Assign as Assign
import SharedLogic.SharedRun.Coverage.Rules
import qualified SharedLogic.SharedRun.Knobs as Knobs
import qualified SharedLogic.SharedRun.Reassign as Reassign
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRouteMapping as QVRM

type CoverageFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    SchedulerJobFlow r,
    OpsNote.OpsNoteFlow m r
  )

-- | The one hook the materialiser calls on a run it has just created: the check-in job (when the city asks drivers
-- to acknowledge) and the fallback job, at the times the city's supply knobs give. Idempotent at the materialiser's
-- level, which never re-creates a run. A city without supply knobs schedules nothing.
scheduleCoverageJobs :: CoverageFlow m r => DTC.TransporterConfig -> DRB.Booking -> m ()
scheduleCoverageJobs transporterConfig booking = whenJust (Knobs.supplyKnobsOf transporterConfig) $ \knobs -> do
  now <- getCurrentTime
  let inSeconds due = max 1 (diffUTCTime due now)
  whenJust (Knobs.checkInDue knobs booking.startTime) $ \due ->
    createJobIn @_ @'SharedRunCheckIn (Just booking.providerId) (Just booking.merchantOperatingCityId) (inSeconds due) $
      SharedRunCheckInJobData {bookingId = booking.id}
  createJobIn @_ @'SharedRunFallback (Just booking.providerId) (Just booking.merchantOperatingCityId) (inSeconds (Knobs.fallbackDue knobs booking.startTime)) $
    SharedRunFallbackJobData {bookingId = booking.id, reason = "scheduled"}

-- | Everything the coverage jobs act on, read fresh.
data RunSnapshot = RunSnapshot
  { booking :: DRB.Booking,
    transporterConfig :: DTC.TransporterConfig,
    knobs :: SharedSupplyKnobs,
    -- | the active ride holding the run, if any (NEW or UPCOMING)
    ride :: Maybe DRide.Ride,
    state :: RunState
  }

-- | Nothing when the run's city no longer has supply knobs (nothing to cover by).
runSnapshot :: CoverageFlow m r => DRB.Booking -> m (Maybe RunSnapshot)
runSnapshot booking = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  forM (Knobs.supplyKnobsOf transporterConfig) $ \knobs -> do
    now <- getCurrentTime
    mbRide <- (>>= \r -> if r.status `elem` [DRide.NEW, DRide.UPCOMING] then Just r else Nothing) <$> QRide.findActiveByRBId booking.id
    assignment <- forM mbRide $ \r -> Assignment <$> Ack.isRunAcknowledged r.id
    let liveRun = DTrip.isSharedRouteTrip booking.tripCategory && booking.status `elem` [DRB.NEW, DRB.TRIP_ASSIGNED]
    pure
      RunSnapshot
        { booking,
          transporterConfig,
          knobs,
          ride = mbRide,
          state =
            RunState
              { now,
                startTime = booking.startTime,
                liveRun,
                assignment,
                checkInDue = Knobs.checkInDue knobs booking.startTime,
                fallbackDue = Knobs.fallbackDue knobs booking.startTime
              }
        }

-- | The owned fleet for the run, in the order the fallback tries it: the drivers of the vehicles bound to the run's
-- route, then the other drivers of those vehicles' fleet owners; minus anyone who dropped the run and anyone whose
-- vehicle does not serve the run's tier (the dashboard assign's check). Blocked route mappings are never read.
ownedFleetCandidates :: CoverageFlow m r => DRB.Booking -> m [Assign.RunCandidate]
ownedFleetCandidates booking = do
  mappings <- case booking.routeCode of
    Nothing -> pure []
    Just routeCode -> filter ((== booking.merchantOperatingCityId) . (.merchantOperatingCityId)) <$> QVRM.findAllByRouteCode routeCode
  bound <- fmap catMaybes . forM mappings $ \mapping -> do
    res <- withTryCatch "sharedRun:boundVehicle" $ do
      plate <- decrypt mapping.vehicleNumber
      vehicle <- QVehicle.findByRegistrationNo plate >>= fromMaybeM (InvalidRequest $ "vehicle " <> plate <> " bound to the run's route is not registered")
      driver <- QPerson.findById vehicle.driverId >>= fromMaybeM (PersonNotFound vehicle.driverId.getId)
      pure (candidateOf BoundVehicle mapping.fleetOwnerId ("the driver of " <> plate) driver (Just vehicle))
    either (\err -> logWarning ("shared run " <> booking.id.getId <> ": skipping a bound vehicle: " <> show err) >> pure Nothing) (pure . Just) res
  fleet <- fmap concat . forM (nub (map (.fleetOwnerId) mappings)) $ \fleetOwnerId -> do
    associations <- QFDA.findAllActiveDriverByFleetOwnerId fleetOwnerId.getId Nothing Nothing Nothing Nothing Nothing Nothing
    forM associations $ \(_, driver) -> do
      mbVehicle <- QVehicle.findById driver.id
      pure (candidateOf FleetDriver fleetOwnerId ("fleet driver " <> driver.id.getId) driver mbVehicle)
  dropped <- Reassign.droppedDrivers booking.id
  let (selected, excluded) = selectCandidates dropped (bound <> fleet)
  unless (null excluded) $
    logInfo $ "shared run " <> booking.id.getId <> ": left out of the fallback: " <> T.intercalate ", " [d.getId <> " (" <> show why <> ")" | (d, why) <- excluded]
  pure (map (.payload) selected)
  where
    candidateOf :: CandidateSource -> Id DP.Person -> Text -> DP.Person -> Maybe DVeh.Vehicle -> Candidate (Id DP.Person) Assign.RunCandidate
    candidateOf source fleetOwnerId label driver mbVehicle =
      Candidate
        { driverId = driver.id,
          source,
          servesTier = maybe False ((booking.vehicleServiceTier `elem`) . (.selectedServiceTiers)) mbVehicle,
          payload = Assign.RunCandidate {driver, fleetOwnerId, label}
        }

-- | Tell ops a run is not covered, at most once per run.
alertOpsOnce :: CoverageFlow m r => DRB.Booking -> Text -> m ()
alertOpsOnce booking note = do
  now <- getCurrentTime
  let ttl = max 60 (ceiling (diffUTCTime (addUTCTime 86400 booking.startTime) now))
  firstAlert <- Hedis.withMasterRedis $ Hedis.setNxExpire ("sharedRun:opsAlerted:" <> booking.id.getId) ttl True
  when firstAlert $ OpsNote.addSystemOpsNote booking note
