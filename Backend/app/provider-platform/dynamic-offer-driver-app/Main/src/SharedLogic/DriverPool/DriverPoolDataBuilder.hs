module SharedLogic.DriverPool.DriverPoolDataBuilder
  ( getOrBuildDriverPoolDataBatch,
    buildDriverPoolDataFromDB,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import Domain.Types.Person (Driver)
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.DriverPool.DriverPoolData
import qualified SharedLogic.DriverPool.DriverPoolMigrations as Migrations
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV

-- | Get pool data for drivers, lazily building from DB for any missing keys.
-- This handles cold start: first call after deploy fetches from DB and caches.
-- Subsequent calls hit Redis directly.
-- Writes are routed to the correct cloud based on each driver's cloudType.
-- The two Bool flags gate what the cold-start builder does with its fetches:
-- bankAccounts is only fetched/consumed when the merchant has online payment
-- enabled; fleet associations are always fetched (cash-ride eligibility needs
-- them unconditionally) but 'DriverPoolData.fleetOwnerId' itself is only
-- populated from them when prepaid wallet flow or online payment is on.
getOrBuildDriverPoolDataBatch ::
  ( BeamFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Bool ->
  Bool ->
  [Id Driver] ->
  m [DriverPoolData]
getOrBuildDriverPoolDataBatch onlinePayment isPrepaidEnabled driverIds = do
  deploymentCloudType <- asks (.cloudType)
  found <- getDriverPoolDataBatch driverIds
  let foundIds = map (.driverId) found
      missing = filter (`notElem` foundIds) driverIds

  migratedFound <- Migrations.applyMigrations found
  let migratedEntries = map fst migratedFound
      toPersist = [e | (e, changed) <- migratedFound, changed]
  unless (null toPersist) $ do
    logInfo $ "DriverPoolData migrations: backfilled " <> show (length toPersist) <> " entries"
    mapM_ (setDriverPoolDataByCloud deploymentCloudType) toPersist

  builtFromDB <-
    if null missing
      then pure []
      else do
        logInfo $ "DriverPoolData cold start: building from DB for " <> show (length missing) <> " drivers"
        b <- buildDriverPoolDataFromDB onlinePayment isPrepaidEnabled missing
        mapM_ (setDriverPoolDataByCloud deploymentCloudType) b
        pure b
  pure $ migratedEntries <> builtFromDB

-- | Build DriverPoolData from DB tables for drivers that don't have a Redis key yet.
-- Always fetches: driver_information, vehicle, person.
-- Conditionally fetches (only when @isPrepaidEnabled || onlinePayment@):
--   - fleet_driver_association / fleet_owner_information — used for @fleetOwnerId@,
--     the fleet-owner bank-account redirect, and the fleet-scoped cash-ride flag.
--   driver_bank_account — only when @onlinePayment@ is true.
--
-- The @isPrepaidEnabled || onlinePayment@ gate is the semantic boundary of the
-- cash-ride toggle: enabling/disabling cash rides is only meaningful when a
-- non-cash payment option exists. With neither flag every ride is a cash ride,
-- so @enableCashRide@ just falls back to driver_information.enableCashRide
-- (default True) and the fleet tables aren't needed.
buildDriverPoolDataFromDB ::
  (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Bool ->
  Bool ->
  [Id Driver] ->
  m [DriverPoolData]
buildDriverPoolDataFromDB onlinePayment isPrepaidEnabled driverIds = do
  let personIds = map cast driverIds :: [Id Person.Person]
      driverIdTexts = map (.getId) driverIds

  driverInfos <- QDI.findAllByDriverIds driverIdTexts
  let diMap = HashMap.fromList $ map (\di -> (di.driverId, di)) driverInfos

  vehicles <- QV.findAllByDriverIds personIds
  let vMap = HashMap.fromList $ map (\v -> (cast v.driverId, v)) vehicles

  persons <- QP.getDriversByIdIn personIds
  let pMap = HashMap.fromList $ map (\p -> (cast p.id, p)) persons

  fleetAssocs <-
    if isPrepaidEnabled || onlinePayment
      then QFDA.findAllByDriverIds personIds
      else pure []
  let faMap = HashMap.fromList $ map (\fa -> (cast fa.driverId, fa)) fleetAssocs
      fleetOwnerPersonIds = DL.nub $ map (\fa -> Id @Person.Person fa.fleetOwnerId) fleetAssocs

  bankAccounts <-
    if onlinePayment
      then QDBA.getDriverBankAccounts (DL.nub (personIds <> fleetOwnerPersonIds))
      else pure []
  let baMap = HashMap.fromList $ map (\ba -> (ba.driverId, ba)) bankAccounts

  fleetOwnerInfos <- if null fleetOwnerPersonIds then pure [] else QFOI.findAllByPrimaryKeys fleetOwnerPersonIds
  let fleetOwnerFOIMap = HashMap.fromList $ map (\foi -> (foi.fleetOwnerPersonId, foi)) fleetOwnerInfos

  now <- getClockTimeInMs
  pure $ mapMaybe (buildOne now diMap vMap pMap baMap faMap fleetOwnerFOIMap) driverIds
  where
    buildOne now diMap vMap pMap baMap faMap fleetOwnerFOIMap did = do
      di <- HashMap.lookup did diMap
      v <- HashMap.lookup did vMap
      p <- HashMap.lookup did pMap
      let fa = HashMap.lookup did faMap
      let effectiveBa = case fa of
            Just assoc -> HashMap.lookup (Id @Person.Person assoc.fleetOwnerId) baMap
            Nothing -> HashMap.lookup (cast did :: Id Person.Person) baMap
      let fleetOwnerEnableCashRide = fa >>= \assoc -> HashMap.lookup (Id @Person.Person assoc.fleetOwnerId) fleetOwnerFOIMap >>= (.enableCashRide)
          associationEnableCashRide = fa >>= (.enableCashRide)
          cashRideEnabled = case fa of
            Just _ -> fromMaybe True fleetOwnerEnableCashRide && fromMaybe True associationEnableCashRide
            Nothing -> fromMaybe True di.enableCashRide
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
            totalRides = Just 0, -- make nothing after first iteration
            variant = v.variant,
            selectedServiceTiers = v.selectedServiceTiers,
            selectedAutoAcceptTiers = v.selectedAutoAcceptTiers,
            enabled = di.enabled,
            blocked = di.blocked,
            isDisabledReasonFlag = isJust di.disabledReasonFlag,
            subscribed = di.subscribed,
            canSwitchToRental = di.canSwitchToRental,
            canSwitchToInterCity = di.canSwitchToInterCity,
            canSwitchToIntraCity = di.canSwitchToIntraCity,
            enableForAirport = Just di.enableForAirport,
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
            enableCashRide = Just cashRideEnabled,
            chargesEnabled = maybe False (.chargesEnabled) effectiveBa,
            bankAccountPaymentMode = (.paymentMode) =<< effectiveBa,
            language = p.language,
            gender = p.gender,
            driverTag = p.driverTag,
            clientDevice = p.clientDevice,
            clientSdkVersion = p.clientSdkVersion,
            clientBundleVersion = p.clientBundleVersion,
            clientConfigVersion = p.clientConfigVersion,
            vehicleTags = v.vehicleTags,
            mYManufacturing = v.mYManufacturing,
            safetyPlusEnabled = Just False,
            fleetOwnerId = (.fleetOwnerId) <$> fa,
            driverTripEndLocation = di.driverTripEndLocation,
            hasRideStarted = di.hasRideStarted,
            airConditionScore = di.airConditionScore,
            airConditioned = v.airConditioned,
            luggageCapacity = v.luggageCapacity,
            vehicleRating = v.vehicleRating,
            registrationNo = v.registrationNo,
            cloudType = p.cloudType,
            schemaVersion = Just Migrations.currentSchemaVersion,
            lastUpdatedAt = Just now
          }
