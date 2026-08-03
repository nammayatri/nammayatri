-- | Filter-driven query plans for the dashboard driver grid.
--
--   The old shape always joined person ⋈ driver_information ⟕ vehicle and filtered afterwards, so
--   every request paid for the full three-table join even when the caller had supplied an exact
--   identifier. Here the table carrying the most selective filter drives the query, the other
--   tables that carry a filter are applied as correlated EXISTS (Postgres plans these as
--   semi-joins, so they cost a join without widening the row or multiplying it), and the tables
--   needed only for the response are fetched afterwards by id in one batch each.
--
--   Driving table, by filter set:
--
--   > no filter                        DI     : Person, FDA, Vehicle
--   > phoneNumber (exact)              Person : DI, FDA, Vehicle
--   >   + approval                     Person . DI : FDA, Vehicle
--   >   + approval + fleetOwnerId      Person . FDA . DI : Vehicle
--   >   + fleetOwnerId                 Person . FDA : DI, Vehicle
--   > name (partial)                   Person : DI, FDA, Vehicle
--   >   + approval                     Person . DI : FDA, Vehicle
--   > vehicleNumber (exact)            Vehicle : Person, DI, FDA
--   >   + approval                     Vehicle . DI : Person, FDA
--   > approval                         DI     : Person, FDA, Vehicle
--   >   + fleetOwnerId                 DI . FDA : Person, Vehicle
--   > fleetOwnerId                     FDA    : Person, DI, Vehicle
--
--   The merchant / operating-city scope is applied to whichever table drives, which is what lets
--   the per-table (city, ...) indexes be used. Ordering follows the driving table's createdAt.
module Storage.Queries.OnboardingList.DriverList
  ( DriverListFilters (..),
    emptyDriverListFilters,
    findDrivers,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Database.Beam as B
import Database.Beam.Postgres (Postgres)
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import Domain.Types.Vehicle (Vehicle)
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.FleetDriverAssociation as BeamFDA
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Vehicle as BeamV
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.VehicleExtra as QVehicleExtra

data DriverListFilters = DriverListFilters
  { dlfVerified :: Maybe Bool,
    dlfEnabled :: Maybe Bool,
    dlfBlocked :: Maybe Bool,
    dlfSubscribed :: Maybe Bool,
    dlfPhoneHash :: Maybe DbHash,
    dlfVehicleNumber :: Maybe Text,
    dlfNameSearch :: Maybe Text,
    dlfApproval :: Maybe (Maybe Bool),
    dlfOnboardingAs :: Maybe DriverInfo.OnboardingAs,
    dlfFleetOwnerId :: Maybe Text,
    dlfFrom :: Maybe UTCTime,
    dlfTo :: Maybe UTCTime
  }

emptyDriverListFilters :: DriverListFilters
emptyDriverListFilters =
  DriverListFilters
    { dlfVerified = Nothing,
      dlfEnabled = Nothing,
      dlfBlocked = Nothing,
      dlfSubscribed = Nothing,
      dlfPhoneHash = Nothing,
      dlfVehicleNumber = Nothing,
      dlfNameSearch = Nothing,
      dlfApproval = Nothing,
      dlfOnboardingAs = Nothing,
      dlfFleetOwnerId = Nothing,
      dlfFrom = Nothing,
      dlfTo = Nothing
    }

data DrivingTable = DriveVehicle | DrivePerson | DriveDriverInfo | DriveFleetAssoc

hasDriverInfoFilter :: DriverListFilters -> Bool
hasDriverInfoFilter f =
  any isJust [f.dlfVerified, f.dlfEnabled, f.dlfBlocked, f.dlfSubscribed]
    || isJust f.dlfApproval
    || isJust f.dlfOnboardingAs

-- from / to are person.createdAt, so a date window keeps Person in the filter set and therefore
-- keeps the ordering on the column the grid has always sorted by.
hasPersonFilter :: DriverListFilters -> Bool
hasPersonFilter f = isJust f.dlfPhoneHash || isJust f.dlfNameSearch || isJust f.dlfFrom || isJust f.dlfTo

drivingTable :: DriverListFilters -> DrivingTable
drivingTable f
  | isJust f.dlfVehicleNumber = DriveVehicle
  | hasPersonFilter f = DrivePerson
  | hasDriverInfoFilter f = DriveDriverInfo
  | isJust f.dlfFleetOwnerId = DriveFleetAssoc
  | otherwise = DriveDriverInfo

personMatches :: Merchant -> DMOC.MerchantOperatingCity -> DriverListFilters -> BeamP.PersonT (B.QGenExpr B.QValueContext Postgres s) -> B.QGenExpr B.QValueContext Postgres s Bool
personMatches merchant opCity f person =
  person.merchantId B.==. B.val_ (getId merchant.id)
    B.&&. cityMatches person.merchantOperatingCityId
    B.&&. maybe (B.val_ True) (\phoneHash -> person.mobileNumberHash B.==. B.val_ (Just phoneHash)) f.dlfPhoneHash
    B.&&. maybe
      (B.val_ True)
      ( \name ->
          let pat = B.val_ ("%" <> name <> "%")
           in B.like_ person.firstName pat
                B.||. B.like_ (B.coalesce_ [person.middleName] (B.val_ "")) pat
                B.||. B.like_ (B.coalesce_ [person.lastName] (B.val_ "")) pat
      )
      f.dlfNameSearch
    B.&&. maybe (B.val_ True) (\fromTime -> person.createdAt B.>=. B.val_ fromTime) f.dlfFrom
    B.&&. maybe (B.val_ True) (\toTime -> person.createdAt B.<=. B.val_ toTime) f.dlfTo
  where
    cityMatches col =
      col B.==. B.val_ (Just $ getId opCity.id)
        B.||. (B.isNothing_ col B.&&. B.val_ (merchant.city == opCity.city))

driverInfoMatches :: DriverListFilters -> BeamDI.DriverInformationT (B.QGenExpr B.QValueContext Postgres s) -> B.QGenExpr B.QValueContext Postgres s Bool
driverInfoMatches f driverInfo =
  maybe (B.val_ True) (\verified -> driverInfo.verified B.==. B.val_ verified) f.dlfVerified
    B.&&. maybe (B.val_ True) (\enabled -> driverInfo.enabled B.==. B.val_ enabled) f.dlfEnabled
    B.&&. maybe (B.val_ True) (\blocked -> driverInfo.blocked B.==. B.val_ blocked) f.dlfBlocked
    B.&&. maybe (B.val_ True) (\subscribed -> driverInfo.subscribed B.==. B.val_ subscribed) f.dlfSubscribed
    B.&&. maybe (B.val_ True) (\onboardingAs -> driverInfo.onboardingAs B.==. B.val_ (Just onboardingAs)) f.dlfOnboardingAs
    B.&&. ( case f.dlfApproval of
              Nothing -> B.val_ True
              Just Nothing -> B.isNothing_ driverInfo.approved
              Just (Just approved) -> driverInfo.approved B.==. B.val_ (Just approved)
          )

-- | Scope applied when driver_information drives, so the (city, verified, approved, enabled) index
--   is usable without reaching into person.
driverInfoScoped :: Merchant -> DMOC.MerchantOperatingCity -> BeamDI.DriverInformationT (B.QGenExpr B.QValueContext Postgres s) -> B.QGenExpr B.QValueContext Postgres s Bool
driverInfoScoped merchant opCity driverInfo =
  driverInfo.merchantId B.==. B.val_ (Just $ getId merchant.id)
    B.&&. ( driverInfo.merchantOperatingCityId B.==. B.val_ (Just $ getId opCity.id)
              B.||. (B.isNothing_ driverInfo.merchantOperatingCityId B.&&. B.val_ (merchant.city == opCity.city))
          )

-- | An association is only current if it has not lapsed: a NULL or past associatedTill compares as
--   NULL / False, so a lapsed row cannot satisfy a fleetOwnerId filter.
fleetAssocMatches :: UTCTime -> DriverListFilters -> BeamFDA.FleetDriverAssociationT (B.QGenExpr B.QValueContext Postgres s) -> B.QGenExpr B.QValueContext Postgres s Bool
fleetAssocMatches now f fda =
  fda.associatedTill B.>. B.val_ (Just now)
    B.&&. maybe (B.val_ True) (\fleetOwnerId -> fda.fleetOwnerId B.==. B.val_ fleetOwnerId) f.dlfFleetOwnerId

vehicleMatches :: Merchant -> DMOC.MerchantOperatingCity -> DriverListFilters -> BeamV.VehicleT (B.QGenExpr B.QValueContext Postgres s) -> B.QGenExpr B.QValueContext Postgres s Bool
vehicleMatches merchant opCity f vehicle =
  vehicle.merchantId B.==. B.val_ (getId merchant.id)
    B.&&. ( vehicle.merchantOperatingCityId B.==. B.val_ (Just $ getId opCity.id)
              B.||. (B.isNothing_ vehicle.merchantOperatingCityId B.&&. B.val_ (merchant.city == opCity.city))
          )
    -- Registration numbers are stored exactly as supplied, so this is an equality lookup rather
    -- than the old LIKE '%..%' scan.
    B.&&. maybe (B.val_ True) (\vehNum -> vehicle.registrationNo B.==. B.val_ vehNum) f.dlfVehicleNumber

-- | The driving query, as a stream of driver ids in page order. Everything the response needs
--   beyond the filters is fetched by id afterwards.
driverIdsQuery ::
  UTCTime ->
  Merchant ->
  DMOC.MerchantOperatingCity ->
  DriverListFilters ->
  B.Q Postgres BeamCommon.AtlasDB s (B.QGenExpr B.QValueContext Postgres s Text, B.QGenExpr B.QValueContext Postgres s UTCTime)
driverIdsQuery now merchant opCity f = case drivingTable f of
  DriveVehicle -> do
    vehicle <- B.filter_ (vehicleMatches merchant opCity f) $ B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)
    applySecondary vehicle.driverId
    pure (vehicle.driverId, vehicle.createdAt)
  DrivePerson -> do
    person <- B.filter_ (personMatches merchant opCity f) $ B.all_ (BeamCommon.person BeamCommon.atlasDB)
    whenFilter (hasDriverInfoFilter f) $ existsDriverInfo person.id
    whenFilter (isJust f.dlfFleetOwnerId) $ existsFleetAssoc person.id
    whenFilter (isJust f.dlfVehicleNumber) $ existsVehicle person.id
    pure (person.id, person.createdAt)
  DriveDriverInfo -> do
    driverInfo <-
      B.filter_ (\di -> driverInfoScoped merchant opCity di B.&&. driverInfoMatches f di) $
        B.all_ (BeamCommon.driverInformation BeamCommon.atlasDB)
    whenFilter (isJust f.dlfFleetOwnerId) $ existsFleetAssoc driverInfo.driverId
    whenFilter (isJust f.dlfVehicleNumber) $ existsVehicle driverInfo.driverId
    pure (driverInfo.driverId, driverInfo.createdAt)
  DriveFleetAssoc -> do
    fda <- B.filter_ (fleetAssocMatches now f) $ B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
    whenFilter (isJust f.dlfVehicleNumber) $ existsVehicle fda.driverId
    pure (fda.driverId, fda.createdAt)
  where
    whenFilter cond q = when cond (B.guard_' (B.sqlBool_ q))
    applySecondary driverId = do
      whenFilter (hasDriverInfoFilter f) $ existsDriverInfo driverId
      whenFilter (isJust f.dlfFleetOwnerId) $ existsFleetAssoc driverId
    existsVehicle driverId =
      B.exists_ $
        B.filter_ (\v -> v.driverId B.==. driverId B.&&. vehicleMatches merchant opCity f v) $
          B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)
    existsDriverInfo driverId =
      B.exists_ $
        B.filter_ (\di -> di.driverId B.==. driverId B.&&. driverInfoMatches f di) $
          B.all_ (BeamCommon.driverInformation BeamCommon.atlasDB)
    existsFleetAssoc driverId =
      B.exists_ $
        B.filter_ (\fda -> fda.driverId B.==. driverId B.&&. fleetAssocMatches now f fda) $
          B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)

findDrivers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Merchant ->
  DMOC.MerchantOperatingCity ->
  Int ->
  Int ->
  DriverListFilters ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findDrivers merchant opCity limitVal offsetVal f = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  result <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limitVal) $
            B.offset_ (fromIntegral offsetVal) $
              B.orderBy_ (\(driverId, createdAt) -> (B.desc_ createdAt, B.asc_ driverId)) $
                driverIdsQuery now merchant opCity f
  let driverIds = either (const []) (map fst) result
  if null driverIds
    then pure []
    else do
      persons <- QPersonExtra.findAllByPersonIds driverIds
      driverInfos <- QDIExtra.findAllByDriverIds driverIds
      vehicles <- QVehicleExtra.findAllByDriverIds (Id <$> driverIds)
      let personById = HM.fromList $ map (\p -> (p.id.getId, p)) persons
          driverInfoById = HM.fromList $ map (\di -> (di.driverId.getId, di)) driverInfos
          vehicleById = HM.fromList $ map (\v -> (v.driverId.getId, v)) vehicles
      -- Rebuilt in the driving query's order; the point fetches come back unordered.
      pure $
        mapMaybe
          ( \driverId -> do
              person <- HM.lookup driverId personById
              driverInfo <- HM.lookup driverId driverInfoById
              pure (person, driverInfo, HM.lookup driverId vehicleById)
          )
          driverIds
