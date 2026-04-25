module SharedLogic.DriverPool.DriverPoolDataBuilder
  ( getOrBuildDriverPoolDataBatch,
    buildDriverPoolDataFromDB,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Domain.Types.Extra.Plan as DExtraPlan
import Domain.Types.Person (Driver)
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.DriverPool.DriverPoolData
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV

-- | Get pool data for drivers, lazily building from DB for any missing keys.
-- This handles cold start: first call after deploy fetches from DB and caches.
-- Subsequent calls hit Redis directly.
getOrBuildDriverPoolDataBatch ::
  (BeamFlow m r, Redis.HedisFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id Driver] ->
  m [DriverPoolData]
getOrBuildDriverPoolDataBatch driverIds = do
  -- Batch fetch from LTS Redis, tracking which are missing
  redisResults <- mapM (\did -> (did,) <$> Redis.safeGet (driverPoolDataKey did)) driverIds
  let (found, missing) = foldr partitionResult ([], []) redisResults
  if null missing
    then pure found
    else do
      -- Fetch missing from DB and populate Redis
      logInfo $ "DriverPoolData cold start: building from DB for " <> show (length missing) <> " drivers"
      builtFromDB <- buildDriverPoolDataFromDB missing
      mapM_ setDriverPoolData builtFromDB
      pure $ found <> builtFromDB
  where
    partitionResult :: (Id Driver, Maybe DriverPoolData) -> ([DriverPoolData], [Id Driver]) -> ([DriverPoolData], [Id Driver])
    partitionResult (_, Just dpd) (founds, missings) = (dpd : founds, missings)
    partitionResult (did, Nothing) (founds, missings) = (founds, did : missings)

-- | Build DriverPoolData from DB tables for drivers that don't have a Redis key yet.
-- Fetches from: driver_information, vehicle, person, driver_bank_account,
-- driver_stats, driver_plan (safety plus), fleet_driver_association.
buildDriverPoolDataFromDB ::
  (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id Driver] ->
  m [DriverPoolData]
buildDriverPoolDataFromDB driverIds = do
  let personIds = map cast driverIds :: [Id Person.Person]
      driverIdTexts = map (.getId) driverIds

  -- Batch fetch all required tables
  driverInfos <- QDI.findAllByDriverIds driverIdTexts
  let diMap = HashMap.fromList $ map (\di -> (di.driverId, di)) driverInfos

  vehicles <- QV.findAllByDriverIds personIds
  let vMap = HashMap.fromList $ map (\v -> (cast v.driverId, v)) vehicles

  persons <- QP.getDriversByIdIn personIds
  let pMap = HashMap.fromList $ map (\p -> (cast p.id, p)) persons

  driverStats <- QDS.findAllByDriverIds persons
  let dsMap = HashMap.fromList $ map (\ds -> (ds.driverId, ds)) driverStats

  bankAccounts <- QDBA.getDriverBankAccounts personIds
  let baMap = HashMap.fromList $ map (\ba -> (cast ba.driverId, ba)) bankAccounts

  fleetAssocs <- QFDA.findAllByDriverIds personIds
  let faMap = HashMap.fromList $ map (\fa -> (cast fa.driverId, fa)) fleetAssocs

  safetyPlusPlans <- mapM (\did -> (did,) <$> QDP.findByDriverIdWithServiceName (cast did) (DExtraPlan.DASHCAM_RENTAL DExtraPlan.CAUTIO)) driverIds
  let spMap = HashMap.fromList safetyPlusPlans

  pure $ mapMaybe (buildOne diMap vMap pMap dsMap baMap faMap spMap) driverIds
  where
    buildOne diMap vMap pMap dsMap baMap faMap spMap did = do
      di <- HashMap.lookup did diMap
      v <- HashMap.lookup did vMap
      p <- HashMap.lookup did pMap
      let ds = HashMap.lookup did dsMap
      let ba = HashMap.lookup did baMap
      let fa = HashMap.lookup did faMap
      let sp = join $ HashMap.lookup did spMap
      Just
        DriverPoolData
          { driverId = did,
            active = di.active,
            mode = di.mode,
            onRide = di.onRide,
            onRideTripCategory = show <$> di.onRideTripCategory,
            hasAdvanceBooking = Just di.hasAdvanceBooking,
            latestScheduledBooking = di.latestScheduledBooking,
            latestScheduledPickup = di.latestScheduledPickup,
            deviceToken = p.deviceToken,
            goHomeStatus = Nothing, -- populated by CachedQueries at runtime
            totalRides = maybe 0 (.totalRides) ds,
            variant = v.variant,
            selectedServiceTiers = v.selectedServiceTiers,
            blocked = di.blocked,
            subscribed = di.subscribed,
            canSwitchToRental = di.canSwitchToRental,
            canSwitchToInterCity = di.canSwitchToInterCity,
            canSwitchToIntraCity = di.canSwitchToIntraCity,
            forwardBatchingEnabled = di.forwardBatchingEnabled,
            isSpecialLocWarrior = di.isSpecialLocWarrior,
            tollRouteBlockedTill = di.tollRouteBlockedTill,
            softBlockStiers = di.softBlockStiers,
            acUsageRestrictionType = Just $ show di.acUsageRestrictionType,
            acRestrictionLiftCount = di.acRestrictionLiftCount,
            tripDistanceMinThreshold = di.tripDistanceMinThreshold,
            tripDistanceMaxThreshold = di.tripDistanceMaxThreshold,
            maxPickupRadius = di.maxPickupRadius,
            isPetModeEnabled = di.isPetModeEnabled,
            chargesEnabled = maybe False (.chargesEnabled) ba,
            language = p.language,
            gender = p.gender,
            driverTag = p.driverTag,
            clientDevice = p.clientDevice,
            clientSdkVersion = p.clientSdkVersion,
            clientBundleVersion = p.clientBundleVersion,
            clientConfigVersion = p.clientConfigVersion,
            vehicleTags = v.vehicleTags,
            mYManufacturing = v.mYManufacturing,
            safetyPlusEnabled = maybe False (.enableServiceUsageCharge) sp,
            fleetOwnerId = (.fleetOwnerId) <$> fa,
            driverTripEndLocation = di.driverTripEndLocation,
            hasRideStarted = di.hasRideStarted,
            airConditionScore = di.airConditionScore,
            airConditioned = v.airConditioned,
            luggageCapacity = v.luggageCapacity,
            vehicleRating = v.vehicleRating,
            registrationNo = v.registrationNo
          }
