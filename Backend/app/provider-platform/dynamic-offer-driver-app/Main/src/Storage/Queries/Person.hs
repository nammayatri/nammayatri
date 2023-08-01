{-# LANGUAGE InstanceSigs #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Storage.Queries.Person where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Mb
-- import qualified Storage.Queries.DriverInformation as QueriesDI

import qualified Database.Beam as B
import Database.Beam.Postgres hiding ((++.))
import qualified Database.Beam.Query ()
import qualified Domain.Types.Booking as Booking
import Domain.Types.Booking.BookingLocation
import Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.DriverInformation as DDI
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.DriverQuote as DriverQuote
import Domain.Types.Merchant
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Domain.Types.Vehicle as DV
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.External.Maps as Maps
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocRepDBFlow)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.GenericPretty
import Kernel.Utils.Version
import Lib.Utils
  ( FromTType' (fromTType'),
    ToTType' (toTType'),
    createWithKV,
    deleteWithKV,
    findAllWithKV,
    findAllWithKvInReplica,
    findAllWithOptionsKV,
    findOneWithKV,
    findOneWithKvInReplica,
    getMasterBeamConfig,
    updateOneWithKV,
  )
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Booking.BookingLocation as BeamBL
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.DriverOnboarding.DriverLicense as BeamDL
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import qualified Storage.Beam.DriverQuote as BeamDQ
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Ride.Table as BeamR
import qualified Storage.Beam.Vehicle as BeamV
import Storage.Queries.Booking ()
import qualified Storage.Queries.DriverLocation as QueriesDL
import qualified Storage.Queries.DriverOnboarding.DriverLicense ()
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation ()
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate ()
import Storage.Queries.DriverQuote ()
import Storage.Queries.Instances.DriverInformation ()
import Storage.Queries.Vehicle ()

create :: (L.MonadFlow m, Log m) => Person.Person -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Person)
findById (Id personId) = findOneWithKV [Se.Is BeamP.id $ Se.Eq personId]

findByIdInReplica :: (L.MonadFlow m, Log m, EsqDBReplicaFlow m r) => Id Person -> m (Maybe Person)
findByIdInReplica (Id personId) = findOneWithKvInReplica [Se.Is BeamP.id $ Se.Eq personId]

data FullDriver = FullDriver
  { person :: Person,
    location :: DriverLocation,
    info :: DriverInformation,
    vehicle :: Vehicle
  }

mkFullDriver :: (Person, DriverLocation, DriverInformation, Vehicle) -> FullDriver
mkFullDriver (p, l, i, v) = FullDriver p l i v

findAllDriversWithInfoAndVehicle ::
  (L.MonadFlow m, Log m) =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId limitVal offsetVal mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString = do
  dbConf <- getMasterBeamConfig
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limitVal) $
          B.offset_ (fromIntegral offsetVal) $
            B.filter_'
              ( \(person, driverInfo, vehicle) ->
                  person.merchantId B.==?. B.val_ (getId merchantId)
                    -- B.&&?. B.sqlBool_ ((\vehicleNumber -> vehicle.registrationNo B.==. B.val_ vehicleNumber) mbVehicleNumberSearchString)
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\verified -> driverInfo.verified B.==?. B.val_ verified) mbVerified
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\enabled -> driverInfo.enabled B.==?. B.val_ enabled) mbEnabled
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\blocked -> driverInfo.blocked B.==?. B.val_ blocked) mbBlocked
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\subscribed -> driverInfo.subscribed B.==?. B.val_ subscribed) mbSubscribed
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.val_ (Just searchStrDBHash)) mbSearchPhoneDBHash
              )
              do
                person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
                driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\info' -> BeamP.id person B.==?. BeamDI.driverId info')
                vehicle <- B.leftJoin_' (B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)) (\veh' -> BeamP.id person B.==?. BeamV.driverId veh')
                pure (person, driverInfo, vehicle)
  case result of
    Right x -> do
      let persons = fmap fst' x
          driverInfos = fmap snd' x
          vehicles = fmap thd' x
      p <- catMaybes <$> mapM fromTType' persons
      di <- catMaybes <$> mapM fromTType' driverInfos
      v <- mapM (maybe (pure Nothing) fromTType') vehicles
      pure $ zip3 p di v
    Left _ -> pure []
  where
    fst' (x, _, _) = x
    snd' (_, y, _) = y
    thd' (_, _, z) = z

getDriversList ::
  (L.MonadFlow m, Log m) =>
  [DriverInformation] ->
  m [Person]
getDriversList driverInfos = findAllWithKV [Se.Is BeamP.id $ Se.In personsKeys]
  where
    personsKeys = getId <$> fetchDriverIDsFromInfo driverInfos

getDriversByIdIn :: (L.MonadFlow m, Log m) => [Id Person] -> m [Person]
getDriversByIdIn personIds = findAllWithKV [Se.Is BeamP.id $ Se.In $ getId <$> personIds]

getDriverInformations ::
  (L.MonadFlow m, Log m) =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInformations driverLocations =
  findAllWithKV
    [ Se.And
        ( [Se.Is BeamDI.active $ Se.Eq True]
            <> [Se.Is BeamDI.driverId $ Se.In personKeys]
        )
    ]
  where
    personKeys = getId <$> fetchDriverIDsFromLocations driverLocations

getDriversWithOutdatedLocationsToMakeInactive :: (L.MonadFlow m, Log m, MonadTime m) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before = do
  driverLocations <- QueriesDL.getDriverLocations before
  driverInfos <- getDriverInformations driverLocations
  getDriversList driverInfos

getDriversWithOutdatedLocationsToMakeInactive' :: (L.MonadFlow m, Log m, MonadTime m) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive' before = do
  driverLocations <- QueriesDL.getDriverLocations before
  driverInfos <- getDriverInfos driverLocations
  drivers <- getDriversList driverInfos
  logDebug $ "GetDriversWithOutdatedLocationsToMakeInactive - DLoc:- " <> show (length driverLocations) <> " DInfo:- " <> show (length driverInfos) <> " Drivers:- " <> show (length drivers)
  return drivers

findAllDriversByIdsFirstNameAsc ::
  (Transactionable m, Functor m, L.MonadFlow m, EsqLocRepDBFlow m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc merchantId driverIds = do
  driverLocs <- QueriesDL.getDriverLocs driverIds merchantId
  driverInfos <- getDriverInfos driverLocs
  vehicle <- getVehicles driverInfos
  drivers <- getDrivers vehicle
  return (linkArrays driverLocs driverInfos vehicle drivers)

linkArrays :: [DriverLocation] -> [DriverInformation] -> [Vehicle] -> [Person] -> [FullDriver]
linkArrays driverLocations driverInformations vehicles persons =
  let personHashMap = buildPersonHashMap persons
      vehicleHashMap = buildVehicleHashMap vehicles
      driverInfoHashMap = buildDriverInfoHashMap driverInformations
   in mapMaybe (buildFullDriver personHashMap vehicleHashMap driverInfoHashMap) driverLocations

linkArrayList :: [DriverLocation] -> [DriverInformation] -> [Vehicle] -> [Person] -> LatLong -> Maybe Variant -> [(Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode)]
linkArrayList driverLocations driverInformations vehicles persons LatLong {..} mbVariant =
  let personHashMap = buildPersonHashMap persons
      driverInfoHashMap = buildDriverInfoHashMap driverInformations
      vehicleHashMap = buildVehicleHashMap vehicles
   in mapMaybe (buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap LatLong {..} mbVariant) driverLocations

buildLocationHashMap :: [DriverLocation] -> HashMap.HashMap Text DriverLocation
buildLocationHashMap driverLocations =
  HashMap.fromList $ map (\loc -> (loc.driverId.getId, loc)) driverLocations

buildPersonHashMap :: [Person] -> HashMap.HashMap Text Person
buildPersonHashMap persons =
  HashMap.fromList $ map (\p -> (p.id.getId, p)) persons

buildVehicleHashMap :: [Vehicle] -> HashMap.HashMap Text Vehicle
buildVehicleHashMap vehicles =
  HashMap.fromList $ map (\v -> (v.driverId.getId, v)) vehicles

buildFullDriver :: HashMap.HashMap Text Person -> HashMap.HashMap Text Vehicle -> HashMap.HashMap Text DriverInformation -> DriverLocation -> Maybe FullDriver
buildFullDriver personHashMap vehicleHashMap driverInfoHashMap location = do
  let driverId' = location.driverId.getId
  person <- HashMap.lookup driverId' personHashMap
  vehicle <- HashMap.lookup driverId' vehicleHashMap
  info <- HashMap.lookup driverId' driverInfoHashMap
  Just $ mkFullDriver (person, location, info, vehicle)

getDriverInfos ::
  (L.MonadFlow m, Log m) =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos driverLocs = do
  findAllWithKV [Se.Is BeamDI.driverId $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromLocations driverLocs

getOnRideStuckDriverIds :: (L.MonadFlow m, Log m) => m [DriverInformation]
getOnRideStuckDriverIds = error ""

getVehicles ::
  (L.MonadFlow m, Log m) =>
  [DriverInformation] ->
  m [Vehicle]
getVehicles driverInfo = findAllWithKV [Se.Is BeamV.driverId $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromInfo driverInfo

getVehiclesInReplica ::
  (L.MonadFlow m, Log m) =>
  [DriverInformation] ->
  m [Vehicle]
getVehiclesInReplica driverInfo = findAllWithKvInReplica [Se.Is BeamV.driverId $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromInfo driverInfo

getDrivers ::
  (L.MonadFlow m, Log m) =>
  [Vehicle] ->
  m [Person]
getDrivers vehicles = findAllWithKV [Se.Is BeamP.id $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromVehicle vehicles

getDriversInReplica ::
  (L.MonadFlow m, Log m) =>
  [Vehicle] ->
  m [Person]
getDriversInReplica vehicles = findAllWithKvInReplica [Se.Is BeamP.id $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromVehicle vehicles

getDriversWithMerchID ::
  (L.MonadFlow m, Log m) =>
  Id Merchant ->
  m [Person]
getDriversWithMerchID (Id merchantId) =
  findAllWithKV
    [ Se.And
        ( [Se.Is BeamP.merchantId $ Se.Eq merchantId]
            <> [Se.Is BeamP.role $ Se.Eq Person.DRIVER]
        )
    ]

getDriverQuote ::
  (L.MonadFlow m, Log m) =>
  [Person] ->
  m [DriverQuote]
getDriverQuote persons =
  findAllWithKV
    [ Se.And [Se.Is BeamDQ.driverId $ Se.In personKeys, Se.Is BeamDQ.status $ Se.Eq DriverQuote.Active]
    ]
  where
    personKeys = getId <$> fetchDriverIDsFromPersons persons

getDriverQuoteInReplica ::
  (L.MonadFlow m, Log m) =>
  [Person] ->
  m [DriverQuote]
getDriverQuoteInReplica persons =
  findAllWithKvInReplica
    [ Se.And [Se.Is BeamDQ.driverId $ Se.In personKeys, Se.Is BeamDQ.status $ Se.Eq DriverQuote.Active]
    ]
  where
    personKeys = getId <$> fetchDriverIDsFromPersons persons

getBookingInfo ::
  (L.MonadFlow m, Log m) =>
  [DriverQuote] ->
  m [Booking.Booking]
getBookingInfo driverQuote =
  findAllWithKV
    [ Se.And [Se.Is BeamB.quoteId $ Se.In personsKeys, Se.Is BeamB.status $ Se.Eq Booking.TRIP_ASSIGNED]
    ]
  where
    personsKeys = fetchDriverIDsTextFromQuote driverQuote

getBookingInfoInReplica ::
  (L.MonadFlow m, Log m) =>
  [DriverQuote] ->
  m [Booking.Booking]
getBookingInfoInReplica driverQuote =
  findAllWithKvInReplica
    [ Se.And [Se.Is BeamB.quoteId $ Se.In personsKeys, Se.Is BeamB.status $ Se.Eq Booking.TRIP_ASSIGNED]
    ]
  where
    personsKeys = fetchDriverIDsTextFromQuote driverQuote

getBookingLocs ::
  (L.MonadFlow m, Log m) =>
  [Booking.Booking] ->
  m [BookingLocation]
getBookingLocs bookings = findAllWithKV [Se.Is BeamBL.id $ Se.In bookingKeys]
  where
    bookingKeys = getId <$> fetchToLocationIDFromBooking bookings

getBookingLocsInReplica ::
  (L.MonadFlow m, Log m) =>
  [Booking.Booking] ->
  m [BookingLocation]
getBookingLocsInReplica bookings = findAllWithKvInReplica [Se.Is BeamBL.id $ Se.In bookingKeys]
  where
    bookingKeys = getId <$> fetchToLocationIDFromBooking bookings

fetchDriverIDsFromDriverQuotes :: [DriverQuote] -> [Id Person]
fetchDriverIDsFromDriverQuotes = map DriverQuote.driverId

fetchToLocationIDFromBooking :: [Booking.Booking] -> [Id BookingLocation]
fetchToLocationIDFromBooking = map (.toLocation.id)

fetchQuoteIdFromDriverQuotes :: [DriverQuote] -> [Text]
fetchQuoteIdFromDriverQuotes = map (.id.getId)

fetchDriverIDsFromPersons :: [Person] -> [Id Person]
fetchDriverIDsFromPersons = map (.id)

fetchDriverIDsFromLocations :: [DriverLocation] -> [Id Person]
fetchDriverIDsFromLocations = map DriverLocation.driverId

fetchDriverIDsFromInfo :: [DriverInformation] -> [Id Person]
fetchDriverIDsFromInfo = map DriverInfo.driverId

fetchDriverIDsFromVehicle :: [Vehicle] -> [Id Person]
fetchDriverIDsFromVehicle = map (.driverId)

fetchDriverIDsTextFromQuote :: [DriverQuote] -> [Text]
fetchDriverIDsTextFromQuote = map (.driverId.getId)

findAllDriverInformationWithSeConditions :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamDI.DriverInformationT] -> m [DriverInformation]
findAllDriverInformationWithSeConditions = findAllWithKV

findAllDriverInformationWithSeConditionsInReplica :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamDI.DriverInformationT] -> m [DriverInformation]
findAllDriverInformationWithSeConditionsInReplica = findAllWithKvInReplica

findAllVehiclesWithSeConditions :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamV.VehicleT] -> m [Vehicle]
findAllVehiclesWithSeConditions = findAllWithKV

findAllBookingsWithSeConditions :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamB.BookingT] -> m [Booking.Booking]
findAllBookingsWithSeConditions = findAllWithKV

findAllDriverQuoteWithSeConditions :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamDQ.DriverQuoteT] -> m [DriverQuote]
findAllDriverQuoteWithSeConditions = findAllWithKV

findAllPersonWithSeConditionsNameAsc :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditionsNameAsc conditions = findAllWithOptionsKV conditions (Se.Asc BeamP.firstName) Nothing Nothing

findAllPersonWithSeConditions :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditions = findAllWithKV

findAllPersonWithSeConditionsInReplica :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditionsInReplica = findAllWithKvInReplica

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int
  }

mkDriverWithRidesCount :: (Person, DriverInformation, Maybe Vehicle, Maybe Int) -> DriverWithRidesCount
mkDriverWithRidesCount (person, info, vehicle, ridesCount) = DriverWithRidesCount {..}

fetchDriverInfoWithRidesCount :: (L.MonadFlow m, MonadTime m, Log m) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe DriverWithRidesCount)
fetchDriverInfoWithRidesCount merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  mbDriverInfo <- fetchDriverInfo merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash
  addRidesCount `mapM` mbDriverInfo
  where
    addRidesCount :: L.MonadFlow m => (Person, DriverInformation, Maybe Vehicle) -> m DriverWithRidesCount
    addRidesCount (person, info, vehicle) = do
      dbConf <- getMasterBeamConfig
      resp <-
        L.runDB dbConf $
          L.findRow $
            B.select $
              B.aggregate_ (\ride -> (B.group_ (BeamR.driverId ride), B.as_ @Int B.countAll_)) $
                B.filter_' (\(BeamR.RideT {driverId, status}) -> driverId B.==?. B.val_ (getId person.id) B.&&?. B.sqlNot_ (B.sqlBool_ (B.in_ status $ B.val_ <$> [Ride.NEW, Ride.CANCELLED]))) $
                  B.all_ (BeamCommon.ride BeamCommon.atlasDB)
      let ridesCount = either (const (Just 0)) (snd <$>) resp
      pure (mkDriverWithRidesCount (person, info, vehicle, ridesCount))

fetchDriverInfo :: (L.MonadFlow m, MonadTime m, Log m) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe (Person, DriverInformation, Maybe Vehicle))
fetchDriverInfo (Id merchantId) mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  dbConf <- getMasterBeamConfig
  now <- getCurrentTime
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(person, driverInfo, vehicle, driverLicense, driverRCAssociation, vehicleRegistrationCertificate) ->
              person.merchantId B.==?. B.val_ merchantId
                B.&&?. person.role B.==?. B.val_ Person.DRIVER
                B.&&?. maybe
                  (B.sqlBool_ $ B.val_ True)
                  ( \(mobileNumberDbHash, mobileCountryCode) ->
                      person.mobileCountryCode B.==?. B.val_ (Just mobileCountryCode)
                        B.&&?. (person.mobileNumberHash B.==?. B.val_ (Just mobileNumberDbHash) B.||?. person.alternateMobileNumberHash B.==?. B.val_ (Just mobileNumberDbHash))
                  )
                  mbMobileNumberDbHashWithCode
                B.&&?. B.sqlBool_ (joinOnlyWhenJustTrue mbVehicleNumber (vehicle.registrationNo B.==. B.val_ mbVehicleNumber))
                B.&&?. B.sqlBool_ (joinOnlyWhenJustTrue mbDlNumberHash (driverLicense.licenseNumberHash B.==. B.val_ mbDlNumberHash))
                B.&&?. B.sqlBool_ (joinOnlyWhenJustTrue mbRcNumberHash (vehicleRegistrationCertificate.certificateNumberHash B.==. B.val_ mbRcNumberHash))
          )
          do
            person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
            driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\info' -> BeamP.id person B.==?. BeamDI.driverId info')
            vehicle <- B.leftJoin_' (B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)) (\veh' -> BeamP.id person B.==?. BeamV.driverId veh')
            driverLicense <- B.leftJoin_' (B.all_ (BeamCommon.driverLicense BeamCommon.atlasDB)) (\dl' -> B.sqlBool_ $ joinOnlyWhenJustFalse mbDlNumberHash (BeamP.id person B.==. BeamDL.driverId dl'))
            driverRCAssociation <- B.leftJoin_' (B.all_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB)) (\drca' -> B.sqlBool_ $ joinOnlyWhenJustFalse mbRcNumberHash (BeamP.id person B.==. BeamDRCA.driverId drca' B.&&. (B.just_ (B.val_ now) B.<. BeamDRCA.associatedTill drca')))
            vehicleRegistrationCertificate <- B.leftJoin_' (B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)) (\vrc' -> B.sqlBool_ $ joinOnlyWhenJustFalse mbRcNumberHash (BeamP.id person B.==. BeamVRC.id vrc'))
            pure (person, driverInfo, vehicle, driverLicense, driverRCAssociation, vehicleRegistrationCertificate)
  res' <- case result of
    Right x -> do
      let persons = fmap fst' x
          driverInfos = fmap snd' x
          vehicles = fmap thd' x
      p <- catMaybes <$> mapM fromTType' persons
      di <- catMaybes <$> mapM fromTType' driverInfos
      v <- mapM (maybe (pure Nothing) fromTType') vehicles
      pure $ zip3 p di v
    Left _ -> pure []
  pure $ listToMaybe res'
  where
    fst' (x, _, _, _, _, _) = x
    snd' (_, x, _, _, _, _) = x
    thd' (_, _, x, _, _, _) = x
    joinOnlyWhenJustFalse mbFilter cond = maybe (B.val_ False) (const cond) mbFilter
    joinOnlyWhenJustTrue mbFilter cond = maybe (B.val_ True) (const cond) mbFilter

findByIdAndRoleAndMerchantId :: (L.MonadFlow m, Log m) => Id Person -> Person.Role -> Id Merchant -> m (Maybe Person)
findByIdAndRoleAndMerchantId (Id pid) role_ (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.id $ Se.Eq pid, Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findAllByMerchantId :: (L.MonadFlow m, Log m) => [Person.Role] -> Id Merchant -> m [Person]
findAllByMerchantId roles (Id merchantId) = findAllWithKV [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.In roles]]

findAllByMerchantIdInReplica :: (L.MonadFlow m, Log m, EsqDBReplicaFlow m r) => [Person.Role] -> Id Merchant -> m [Person]
findAllByMerchantIdInReplica roles (Id merchantId) = findAllWithKvInReplica [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.In roles]]

findAdminsByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m [Person]
findAdminsByMerchantId (Id merchantId) = findAllWithKV [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.Eq Person.ADMIN]]

findByMobileNumberAndMerchant :: (L.MonadFlow m, Log m) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchant countryCode mobileNumberHash (Id merchantId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Is BeamP.merchantId $ Se.Eq merchantId,
          Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash]
        ]
    ]

findByMobileNumberAndMerchantInReplica :: (L.MonadFlow m, Log m, EsqDBReplicaFlow m r) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchantInReplica countryCode mobileNumberHash (Id merchantId) =
  findOneWithKvInReplica
    [ Se.And
        [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Is BeamP.merchantId $ Se.Eq merchantId,
          Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash]
        ]
    ]

findByIdentifierAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByIdentifierAndMerchant (Id merchantId) identifier_ = findOneWithKV [Se.And [Se.Is BeamP.identifier $ Se.Eq $ Just identifier_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByEmailAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByEmailAndMerchant (Id merchantId) email_ = findOneWithKV [Se.And [Se.Is BeamP.email $ Se.Eq $ Just email_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByRoleAndMobileNumberAndMerchantId :: (L.MonadFlow m, Log m, EncFlow m r) => Role -> Text -> Text -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber (Id merchantId) = do
  mobileNumberDbHash <- getDbHash mobileNumber
  findOneWithKV
    [ Se.And
        [ Se.Is BeamP.role $ Se.Eq role_,
          Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberDbHash,
          Se.Is BeamP.merchantId $ Se.Eq merchantId
        ]
    ]

-- personDriverTable ::
--   From
--     ( Table PersonT
--         :& Table DriverInformationT
--     )
-- personDriverTable =
--   table @PersonT
--     `innerJoin` table @DriverInformationT
--     `Esq.on` ( \(person :& driver) ->
--                  person ^. PersonTId ==. driver ^. DriverInformationDriverId
--                    &&. Esq.not_ (driver ^. DriverInformationBlocked)
--              )

findAllDriverIdExceptProvided :: (L.MonadFlow m, Log m) => Id Merchant -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvided (Id merchantId) driverIdsToBeExcluded = do
  let personSeCondition = [Se.Is BeamP.merchantId $ Se.Eq merchantId]
  person <- findAllPersonWithSeConditions personSeCondition
  let diSeCondition =
        [ Se.And
            ( [Se.Is BeamDI.driverId $ Se.Not $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> person]
                <> [Se.Is BeamDI.verified $ Se.Eq True]
                <> [Se.Is BeamDI.enabled $ Se.Eq True]
                <> [Se.Is BeamDI.driverId $ Se.Not $ Se.In $ getId <$> driverIdsToBeExcluded]
            )
        ]
  infoList <- findAllDriverInformationWithSeConditions diSeCondition
  pure (map (personIdToDrivrId . DDI.driverId) infoList)
  where
    personIdToDrivrId :: Id Person -> Id Driver
    personIdToDrivrId = cast

findAllDriverIdExceptProvidedInReplica :: (L.MonadFlow m, Log m) => Id Merchant -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvidedInReplica (Id merchantId) driverIdsToBeExcluded = do
  let personSeCondition = [Se.Is BeamP.merchantId $ Se.Eq merchantId]
  person <- findAllPersonWithSeConditionsInReplica personSeCondition
  let diSeCondition =
        [ Se.And
            ( [Se.Is BeamDI.driverId $ Se.Not $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> person]
                <> [Se.Is BeamDI.verified $ Se.Eq True]
                <> [Se.Is BeamDI.enabled $ Se.Eq True]
                <> [Se.Is BeamDI.driverId $ Se.Not $ Se.In $ getId <$> driverIdsToBeExcluded]
            )
        ]
  infoList <- findAllDriverInformationWithSeConditionsInReplica diSeCondition
  pure (map (personIdToDrivrId . DDI.driverId) infoList)
  where
    personIdToDrivrId :: Id Person -> Id Driver
    personIdToDrivrId = cast

updateMerchantIdAndMakeAdmin :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Id Merchant -> m ()
updateMerchantIdAndMakeAdmin (Id personId) (Id merchantId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.merchantId merchantId,
      Se.Set BeamP.role Person.ADMIN,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateName :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Text -> m ()
updateName (Id personId) name = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.firstName name,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatePersonRec :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Person -> m ()
updatePersonRec (Id personId) person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.firstName $ person.firstName,
      Se.Set BeamP.middleName $ person.middleName,
      Se.Set BeamP.lastName $ person.lastName,
      Se.Set BeamP.gender $ person.gender,
      Se.Set BeamP.email $ person.email,
      Se.Set BeamP.hometown $ person.hometown,
      Se.Set BeamP.languagesSpoken $ person.languagesSpoken,
      Se.Set BeamP.identifier $ person.identifier,
      Se.Set BeamP.rating $ person.rating,
      Se.Set BeamP.language $ person.language,
      Se.Set BeamP.deviceToken $ person.deviceToken,
      Se.Set BeamP.merchantId $ getId person.merchantId,
      Se.Set BeamP.description $ person.description,
      Se.Set BeamP.updatedAt now,
      Se.Set BeamP.clientVersion (versionToText <$> person.clientVersion),
      Se.Set BeamP.bundleVersion (versionToText <$> person.bundleVersion)
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatePersonVersions :: (L.MonadFlow m, MonadTime m, Log m) => Person -> Maybe Version -> Maybe Version -> m ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      updateOneWithKV
        [ Se.Set BeamP.clientVersion mbClientVersionText,
          Se.Set BeamP.bundleVersion mbBundleVersionText,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq $ getId person.id)]

updateDeviceToken :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Maybe FCMRecipientToken -> m ()
updateDeviceToken (Id personId) mbDeviceToken = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.deviceToken mbDeviceToken,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateWhatsappNotificationEnrollStatus :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Maybe Whatsapp.OptApiMethods -> m ()
updateWhatsappNotificationEnrollStatus (Id personId) enrollStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.whatsappNotificationEnrollStatus enrollStatus,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m, Log m, EncFlow m r) => Person -> m ()
updateMobileNumberAndCode person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
      Se.Set BeamP.mobileNumberEncrypted $ person.mobileNumber <&> unEncrypted . (.encrypted),
      Se.Set BeamP.mobileNumberHash $ person.mobileNumber <&> (.hash),
      Se.Set BeamP.unencryptedMobileNumber $ person.unencryptedMobileNumber,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq $ getId person.id)]

setIsNewFalse :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m ()
setIsNewFalse (Id personId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.isNew False,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

deleteById :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteById (Id personId) = deleteWithKV [Se.Is BeamP.id (Se.Eq personId)]

updateAverageRating :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Centesimal -> m ()
updateAverageRating (Id personId) newAverageRating = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.rating (Just newAverageRating),
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

data NearestDriversResult = NearestDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    variant :: DV.Variant,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

getNearestDrivers ::
  (Transactionable m, MonadTime m, L.MonadFlow m, EsqDBReplicaFlow m r, EsqLocRepDBFlow m r) =>
  Maybe Variant ->
  LatLong ->
  Int ->
  Id Merchant ->
  Bool ->
  Maybe Seconds ->
  m [NearestDriversResult]
getNearestDrivers mbVariant LatLong {..} radiusMeters merchantId onlyNotOnRide mbDriverPositionInfoExpiry = do
  res <- do
    driverLocs <- QueriesDL.getDriverLocsFromMerchId mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId
    driverInfos <- getDriverInfosWithCond driverLocs onlyNotOnRide False
    vehicle <- getVehiclesWithCond driverInfos
    drivers <- getDrivers vehicle
    return (linkArrayList driverLocs driverInfos vehicle drivers LatLong {..} mbVariant)

  return (makeNearestDriversResult =<< res)
  where
    makeNearestDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode) -> [NearestDriversResult]
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dist, dlat, dlon, variant, mode) = do
      case mbVariant of
        Nothing -> do
          let autoResult = getResult AUTO_RICKSHAW $ variant == AUTO_RICKSHAW
              suvResult = getResult SUV $ variant == SUV
              sedanResult = getResult SEDAN $ variant == SEDAN || (variant == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ variant == HATCHBACK || ((variant == SUV || variant == SEDAN) && canDowngradeToHatchback)
              taxiPlusResult = getResult TAXI_PLUS $ variant == TAXI_PLUS
              taxiResult = getResult TAXI $ variant == TAXI || (variant == TAXI_PLUS && canDowngradeToTaxi)
          autoResult <> suvResult <> sedanResult <> hatchbackResult <> taxiResult <> taxiPlusResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestDriversResult (cast personId) mbDeviceToken mblang onRide (roundToIntegral dist) var dlat dlon mode | cond]

buildFullDriverList :: HashMap.HashMap Text Person -> HashMap.HashMap Text Vehicle -> HashMap.HashMap Text DriverInformation -> LatLong -> Maybe Variant -> DriverLocation -> Maybe (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode)
buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap LatLong {..} mbVariant location = do
  let driverId' = location.driverId.getId
  person <- HashMap.lookup driverId' personHashMap
  vehicle <- HashMap.lookup driverId' vehicleHashMap
  info <- HashMap.lookup driverId' driverInfoHashMap
  let dist = realToFrac $ distanceBetweenInMeters LatLong {..} LatLong {lat = location.lat, lon = location.lon}
  if Mb.isNothing mbVariant || Just vehicle.variant == mbVariant
    || ( case mbVariant of
           Just SEDAN ->
             info.canDowngradeToSedan
               && vehicle.variant == SUV
           Just HATCHBACK ->
             info.canDowngradeToHatchback
               && (vehicle.variant == SUV || vehicle.variant == SEDAN)
           Just TAXI ->
             info.canDowngradeToTaxi
               && vehicle.variant == TAXI_PLUS
           _ -> False
       )
    then Just (person.id, person.deviceToken, person.language, info.onRide, info.canDowngradeToSedan, info.canDowngradeToHatchback, info.canDowngradeToTaxi, dist, location.lat, location.lon, vehicle.variant, info.mode)
    else Nothing

getDriverLocsWithCond ::
  (L.MonadFlow m, MonadTime m, EsqLocRepDBFlow m r) =>
  Id Merchant ->
  Maybe Seconds ->
  LatLong ->
  Int ->
  m [DriverLocation]
getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry LatLong {..} radiusMeters = QueriesDL.getDriverLocsFromMerchId mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId

getDriverInfosWithCond :: (L.MonadFlow m, Log m) => [DriverLocation] -> Bool -> Bool -> m [DriverInformation]
getDriverInfosWithCond driverLocs onlyNotOnRide onlyOnRide =
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamDI.driverId $ Se.In personsKeys,
            Se.Or
              [ Se.And
                  [ Se.Is BeamDI.mode $ Se.Eq Nothing,
                    Se.Is BeamDI.active $ Se.Eq True
                  ],
                Se.And
                  [ Se.Is BeamDI.mode $ Se.Not $ Se.Eq Nothing,
                    Se.Or
                      [ Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.SILENT,
                        Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.ONLINE
                      ]
                  ]
              ],
            Se.Is BeamDI.blocked $ Se.Eq False
          ]
            <> if onlyNotOnRide then [Se.Is BeamDI.onRide $ Se.Eq False] else ([Se.Is BeamDI.onRide $ Se.Eq True | onlyOnRide])
        )
    ]
  where
    personsKeys = getId . cast <$> fetchDriverIDsFromLocations driverLocs

getDriverInfosWithCondInReplica :: (L.MonadFlow m, Log m) => [DriverLocation] -> Bool -> Bool -> m [DriverInformation]
getDriverInfosWithCondInReplica driverLocs onlyNotOnRide onlyOnRide =
  findAllWithKvInReplica
    [ Se.And
        ( [ Se.Is BeamDI.driverId $ Se.In personsKeys,
            Se.Or
              [ Se.And
                  [ Se.Is BeamDI.mode $ Se.Eq Nothing,
                    Se.Is BeamDI.active $ Se.Eq True
                  ],
                Se.And
                  [ Se.Is BeamDI.mode $ Se.Not $ Se.Eq Nothing,
                    Se.Or
                      [ Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.SILENT,
                        Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.ONLINE
                      ]
                  ]
              ],
            Se.Is BeamDI.blocked $ Se.Eq False
          ]
            <> if onlyNotOnRide then [Se.Is BeamDI.onRide $ Se.Eq False] else ([Se.Is BeamDI.onRide $ Se.Eq True | onlyOnRide])
        )
    ]
  where
    personsKeys = getId . cast <$> fetchDriverIDsFromLocations driverLocs

getVehiclesWithCond ::
  (L.MonadFlow m, Log m) =>
  [DriverInformation] ->
  m [Vehicle]
getVehiclesWithCond driverInfo = findAllWithKV [Se.Is BeamV.driverId $ Se.In personsKeys]
  where
    personsKeys = getId <$> fetchDriverIDsFromInfo driverInfo

data NearestDriversResultCurrentlyOnRide = NearestDriversResultCurrentlyOnRide
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    lat :: Double,
    lon :: Double,
    variant :: DV.Variant,
    destinationLat :: Double,
    destinationLon :: Double,
    distanceToDriver :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DriverInfo.DriverMode
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

getNearestDriversCurrentlyOnRide ::
  (Transactionable m, MonadTime m, L.MonadFlow m, EsqDBReplicaFlow m r, EsqLocRepDBFlow m r) =>
  Maybe Variant ->
  LatLong ->
  Int ->
  Id Merchant ->
  Maybe Seconds ->
  Int ->
  m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide mbVariant LatLong {..} radiusMeters merchantId mbDriverPositionInfoExpiry reduceRadiusValue = do
  let onRideRadius = fromIntegral (radiusMeters - reduceRadiusValue) :: Double
  res <- do
    driverLocs <- QueriesDL.getDriverLocsFromMerchId mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId
    driverInfos <- getDriverInfosWithCond driverLocs False True
    vehicles <- getVehicles driverInfos
    drivers <- getDrivers vehicles
    driverQuote <- getDriverQuote drivers
    bookingInfo <- getBookingInfo driverQuote
    bookingLocation <- getBookingLocs bookingInfo

    return (linkArrayListForOnRide driverQuote bookingInfo bookingLocation driverLocs driverInfos vehicles drivers LatLong {..} onRideRadius mbVariant)

  return (makeNearestDriversResult =<< res)
  where
    makeNearestDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Variant, Double, Double, Double, Double, Maybe DriverInfo.DriverMode) -> [NearestDriversResultCurrentlyOnRide]
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dlat, dlon, variant, destinationEndLat, destinationEndLon, dist :: Double, distanceFromDriverToDestination :: Double, mode) =
      case mbVariant of
        Nothing -> do
          let autoResult = getResult AUTO_RICKSHAW $ variant == AUTO_RICKSHAW
              suvResult = getResult SUV $ variant == SUV
              sedanResult = getResult SEDAN $ variant == SEDAN || (variant == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ variant == HATCHBACK || ((variant == SUV || variant == SEDAN) && canDowngradeToHatchback)
              taxiPlusResult = getResult TAXI_PLUS $ variant == TAXI_PLUS
              taxiResult = getResult TAXI $ variant == TAXI || (variant == TAXI_PLUS && canDowngradeToTaxi)
          autoResult <> suvResult <> sedanResult <> hatchbackResult <> taxiResult <> taxiPlusResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestDriversResultCurrentlyOnRide (cast personId) mbDeviceToken mblang onRide dlat dlon var destinationEndLat destinationEndLon (roundToIntegral dist) (roundToIntegral distanceFromDriverToDestination) mode | cond]

getNearestDriversCurrentlyOnRideInReplica ::
  (Transactionable m, MonadTime m, L.MonadFlow m, EsqDBReplicaFlow m r, EsqLocRepDBFlow m r) =>
  Maybe Variant ->
  LatLong ->
  Int ->
  Id Merchant ->
  Maybe Seconds ->
  Int ->
  m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRideInReplica mbVariant LatLong {..} radiusMeters merchantId mbDriverPositionInfoExpiry reduceRadiusValue = do
  let onRideRadius = fromIntegral (radiusMeters - reduceRadiusValue) :: Double
  res <- do
    driverLocs <- QueriesDL.getDriverLocsFromMerchIdInReplica mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId
    driverInfos <- getDriverInfosWithCondInReplica driverLocs False True
    vehicles <- getVehiclesInReplica driverInfos
    drivers <- getDriversInReplica vehicles
    driverQuote <- getDriverQuoteInReplica drivers
    bookingInfo <- getBookingInfoInReplica driverQuote
    bookingLocation <- getBookingLocsInReplica bookingInfo

    return (linkArrayListForOnRide driverQuote bookingInfo bookingLocation driverLocs driverInfos vehicles drivers LatLong {..} onRideRadius mbVariant)

  return (makeNearestDriversResult =<< res)
  where
    makeNearestDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Variant, Double, Double, Double, Double, Maybe DriverInfo.DriverMode) -> [NearestDriversResultCurrentlyOnRide]
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dlat, dlon, variant, destinationEndLat, destinationEndLon, dist :: Double, distanceFromDriverToDestination :: Double, mode) =
      case mbVariant of
        Nothing -> do
          let autoResult = getResult AUTO_RICKSHAW $ variant == AUTO_RICKSHAW
              suvResult = getResult SUV $ variant == SUV
              sedanResult = getResult SEDAN $ variant == SEDAN || (variant == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ variant == HATCHBACK || ((variant == SUV || variant == SEDAN) && canDowngradeToHatchback)
              taxiPlusResult = getResult TAXI_PLUS $ variant == TAXI_PLUS
              taxiResult = getResult TAXI $ variant == TAXI || (variant == TAXI_PLUS && canDowngradeToTaxi)
          autoResult <> suvResult <> sedanResult <> hatchbackResult <> taxiResult <> taxiPlusResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestDriversResultCurrentlyOnRide (cast personId) mbDeviceToken mblang onRide dlat dlon var destinationEndLat destinationEndLon (roundToIntegral dist) (roundToIntegral distanceFromDriverToDestination) mode | cond]

linkArrayListForOnRide :: [DriverQuote] -> [Booking.Booking] -> [BookingLocation] -> [DriverLocation] -> [DriverInformation] -> [Vehicle] -> [Person] -> LatLong -> Double -> Maybe Variant -> [(Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Variant, Double, Double, Double, Double, Maybe DriverInfo.DriverMode)]
linkArrayListForOnRide driverQuotes bookings bookingLocs driverLocations driverInformations vehicles persons LatLong {..} onRideRadius mbVariant =
  let locationHashMap = buildLocationHashMap driverLocations
      personHashMap = buildPersonHashMap persons
      quotesHashMap = buildQuotesHashMap driverQuotes
      bookingHashMap = buildBookingHashMap bookings
      bookingLocsHashMap = buildBookingLocsHashMap bookingLocs
      driverInfoHashMap = buildDriverInfoHashMap driverInformations
   in mapMaybe (buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap LatLong {..} onRideRadius mbVariant) vehicles

buildDriverInfoHashMap :: [DriverInformation] -> HashMap.HashMap Text DriverInformation
buildDriverInfoHashMap driverInfo =
  HashMap.fromList $ map (\info -> (info.driverId.getId, info)) driverInfo

buildQuotesHashMap :: [DriverQuote] -> HashMap.HashMap Text DriverQuote
buildQuotesHashMap driverQuote =
  HashMap.fromList $ map (\quote -> (quote.driverId.getId, quote)) driverQuote

buildBookingHashMap :: [Booking.Booking] -> HashMap.HashMap Text Booking.Booking
buildBookingHashMap bookings =
  HashMap.fromList $ map (\booking -> (booking.quoteId, booking)) bookings

buildBookingLocsHashMap :: [BookingLocation] -> HashMap.HashMap Text BookingLocation
buildBookingLocsHashMap bookinglocs =
  HashMap.fromList $ map (\loc -> (loc.id.getId, loc)) bookinglocs

buildFullDriverListOnRide :: HashMap.HashMap Text DriverQuote -> HashMap.HashMap Text Booking.Booking -> HashMap.HashMap Text BookingLocation -> HashMap.HashMap Text DriverLocation -> HashMap.HashMap Text DriverInformation -> HashMap.HashMap Text Person -> LatLong -> Double -> Maybe Variant -> Vehicle -> Maybe (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Variant, Double, Double, Double, Double, Maybe DriverInfo.DriverMode)
buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap latLon onRideRadius mbVariant vehicle = do
  let driverId' = vehicle.driverId.getId
  location <- HashMap.lookup driverId' locationHashMap
  quote <- HashMap.lookup driverId' quotesHashMap
  booking <- HashMap.lookup quote.id.getId bookingHashMap
  bookingLocation <- HashMap.lookup booking.toLocation.id.getId bookingLocsHashMap
  info <- HashMap.lookup driverId' driverInfoHashMap
  person <- HashMap.lookup driverId' personHashMap
  let driverLocationPoint = LatLong {lat = location.lat, lon = location.lon}
      destinationPoint = LatLong {lat = bookingLocation.lat, lon = bookingLocation.lon}
      distanceFromDriverToDestination = realToFrac $ distanceBetweenInMeters driverLocationPoint destinationPoint
      distanceFromDestinationToPickup = realToFrac $ distanceBetweenInMeters latLon destinationPoint
      onRideRadiusValidity = (distanceFromDriverToDestination + distanceFromDestinationToPickup) < onRideRadius
  if onRideRadiusValidity
    && ( Mb.isNothing mbVariant || Just vehicle.variant == mbVariant
           || ( case mbVariant of
                  Just SEDAN ->
                    info.canDowngradeToSedan
                      && vehicle.variant == SUV
                  Just HATCHBACK ->
                    info.canDowngradeToHatchback
                      && (vehicle.variant == SUV || vehicle.variant == SEDAN)
                  Just TAXI ->
                    info.canDowngradeToTaxi
                      && vehicle.variant == TAXI_PLUS
                  _ -> False
              )
       )
    then Just (person.id, person.deviceToken, person.language, info.onRide, info.canDowngradeToSedan, info.canDowngradeToHatchback, info.canDowngradeToTaxi, location.lat, location.lon, vehicle.variant, bookingLocation.lat, bookingLocation.lon, distanceFromDriverToDestination + distanceFromDestinationToPickup, distanceFromDriverToDestination, info.mode)
    else Nothing

updateAlternateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m, Log m) => Person -> m ()
updateAlternateMobileNumberAndCode person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.alternateMobileNumberEncrypted (person.alternateMobileNumber <&> unEncrypted . (.encrypted)),
      Se.Set BeamP.unencryptedAlternateMobileNumber person.unencryptedAlternateMobileNumber,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq $ getId person.id)]

findAllPersonWithDriverInfos :: (L.MonadFlow m, Log m) => [DriverInformation] -> Id Merchant -> m [Person]
findAllPersonWithDriverInfos dInfos merchantId = findAllWithKV [Se.And [Se.Is BeamP.id $ Se.In (getId . DriverInfo.driverId <$> dInfos), Se.Is BeamP.merchantId $ Se.Eq (getId merchantId)]]

instance FromTType' BeamP.Person Person where
  fromTType' :: (MonadThrow m, L.MonadFlow m, Log m) => BeamP.Person -> m (Maybe Person)
  fromTType' BeamP.PersonT {..} = do
    bundleVersion' <- forM bundleVersion readVersion
    clientVersion' <- forM clientVersion readVersion
    pure $
      Just
        Person
          { id = Id id,
            firstName = firstName,
            middleName = middleName,
            lastName = lastName,
            role = role,
            gender = gender,
            hometown = hometown,
            languagesSpoken = languagesSpoken,
            identifierType = identifierType,
            email = email,
            unencryptedMobileNumber = unencryptedMobileNumber,
            mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
            onboardedFromDashboard = onboardedFromDashboard,
            mobileCountryCode = mobileCountryCode,
            passwordHash = passwordHash,
            identifier = identifier,
            rating = rating,
            isNew = isNew,
            merchantId = Id merchantId,
            deviceToken = deviceToken,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
            language = language,
            description = description,
            createdAt = createdAt,
            updatedAt = updatedAt,
            bundleVersion = bundleVersion',
            clientVersion = clientVersion',
            unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
            alternateMobileNumber = EncryptedHashed <$> (Encrypted <$> alternateMobileNumberEncrypted) <*> alternateMobileNumberHash
          }

instance ToTType' BeamP.Person Person where
  toTType' Person {..} = do
    BeamP.PersonT
      { BeamP.id = getId id,
        BeamP.firstName = firstName,
        BeamP.middleName = middleName,
        BeamP.lastName = lastName,
        BeamP.role = role,
        BeamP.gender = gender,
        BeamP.hometown = hometown,
        BeamP.languagesSpoken = languagesSpoken,
        BeamP.identifierType = identifierType,
        BeamP.email = email,
        BeamP.unencryptedMobileNumber = unencryptedMobileNumber,
        BeamP.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        BeamP.onboardedFromDashboard = onboardedFromDashboard,
        BeamP.mobileNumberHash = mobileNumber <&> (.hash),
        BeamP.mobileCountryCode = mobileCountryCode,
        BeamP.passwordHash = passwordHash,
        BeamP.identifier = identifier,
        BeamP.rating = rating,
        BeamP.isNew = isNew,
        BeamP.merchantId = getId merchantId,
        BeamP.deviceToken = deviceToken,
        BeamP.whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
        BeamP.language = language,
        BeamP.description = description,
        BeamP.createdAt = createdAt,
        BeamP.updatedAt = updatedAt,
        BeamP.bundleVersion = versionToText <$> bundleVersion,
        BeamP.clientVersion = versionToText <$> clientVersion,
        BeamP.unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
        BeamP.alternateMobileNumberHash = alternateMobileNumber <&> (.hash),
        BeamP.alternateMobileNumberEncrypted = alternateMobileNumber <&> unEncrypted . (.encrypted)
      }
