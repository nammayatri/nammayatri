{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Storage.Queries.Person where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Mb
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
import Domain.Types.Ride as Ride
import Domain.Types.Vehicle as DV
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import Kernel.External.Maps as Maps
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.GenericPretty
import Kernel.Utils.Version
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Booking.BookingLocation as BeamBL
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.DriverLocation as BeamDL
import qualified Storage.Beam.DriverQuote as BeamDQ
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Vehicle as BeamV
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking.BookingLocation as QueriesBL
import qualified Storage.Queries.DriverInformation as QueriesDI
import qualified Storage.Queries.DriverLocation as QDL
import qualified Storage.Queries.DriverLocation as QueriesDL
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverQuote as QueriesDQ
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.Vehicle as QueriesV
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person as TPerson
import Storage.Tabular.Ride
import Storage.Tabular.Vehicle as Vehicle

create :: L.MonadFlow m => Person.Person -> m (MeshResult BeamP.Person)
create person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWithKVConnector dbConf' updatedMeshConfig (transformDomainPersonToBeam person)
    Nothing -> pure $ Left $ MKeyNotFound "DB Config not found"

findById :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Person)
findById (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamP.id $ Se.Eq personId]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

data FullDriver = FullDriver
  { person :: Person,
    location :: DriverLocation,
    info :: DriverInformation,
    vehicle :: Vehicle
  }

mkFullDriver :: (Person, DriverLocation, DriverInformation, Vehicle) -> FullDriver
mkFullDriver (p, l, i, v) = FullDriver p l i v

findAllDriversWithInfoAndVehicle ::
  Transactionable m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId limitVal offsetVal mbVerified mbEnabled mbBlocked mbSearchPhoneDBHash mbVehicleNumberSearchString = do
  Esq.findAll $ do
    person :& info :& mbVeh <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
            `Esq.on` ( \(person :& driverInfo) ->
                         person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
                     )
          `leftJoin` table @VehicleT
            `Esq.on` ( \(person :& _ :& mbVehicle) ->
                         just (person ^. PersonTId) ==. mbVehicle ?. VehicleDriverId
                     )
    where_ $
      person ^. PersonMerchantId ==. (val . toKey $ merchantId)
        &&. person ^. PersonRole ==. val Person.DRIVER
        &&. maybe (val True) (\vehicleNumber -> mbVeh ?. VehicleRegistrationNo `Esq.like` just (val vehicleNumber)) mbVehicleNumberSearchString
        &&. maybe (val True) (\verified -> info ^. DriverInformationVerified ==. val verified) mbVerified
        &&. maybe (val True) (\enabled -> info ^. DriverInformationEnabled ==. val enabled) mbEnabled
        &&. maybe (val True) (\blocked -> info ^. DriverInformationBlocked ==. val blocked) mbBlocked
        &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    pure (person, info, mbVeh)

getDriversList ::
  (L.MonadFlow m, Log m) =>
  [DriverInformation] ->
  m [Person]
getDriversList driverInfos = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamP.id $ Se.In personsKeys]
      case result of
        Right result' -> catMaybes <$> mapM transformBeamPersonToDomain result'
        _ -> pure []
    Nothing -> pure []
  where
    personsKeys = getId <$> fetchDriverIDsFromInfo driverInfos

getDriverInformations ::
  L.MonadFlow m =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInformations driverLocations = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              ( [Se.Is BeamDI.active $ Se.Eq True]
                  <> [Se.Is BeamDI.driverId $ Se.In personKeys]
              )
          ]
      case result of
        Left _ -> pure []
        Right result' -> pure $ QueriesDI.transformBeamDriverInformationToDomain <$> result'
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromLocations driverLocations

getDriverLocations ::
  L.MonadFlow m =>
  UTCTime ->
  m [DriverLocation]
getDriverLocations before = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamDL.updatedAt $ Se.LessThan before]
      case result of
        Left _ -> pure []
        Right result' -> pure $ QueriesDL.transformBeamDriverLocationToDomain <$> result'
    Nothing -> pure []

getDriversWithOutdatedLocationsToMakeInactive :: (L.MonadFlow m, Log m, MonadTime m) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before = do
  driverLocations <- QueriesDL.getDriverLocations before
  driverInfos <- getDriverInformations driverLocations
  getDriversList driverInfos

findAllDriversByIdsFirstNameAsc ::
  (Transactionable m, Functor m, L.MonadFlow m) =>
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

getDriverLocs ::
  L.MonadFlow m =>
  [Id Person] ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocs driverIds (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      driverLocs <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              ( [Se.Is BeamDL.merchantId $ Se.Eq merchantId]
                  <> [Se.Is BeamDL.driverId $ Se.In personKeys]
              )
          ]
      case driverLocs of
        Left _ -> pure []
        Right driverLocs' -> pure $ QDL.transformBeamDriverLocationToDomain <$> driverLocs'
    Nothing -> pure []
  where
    personKeys = getId <$> driverIds

getDriverInfos ::
  L.MonadFlow m =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos driverLocs = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      driverInfos <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamDI.driverId $ Se.In personKeys]
      case driverInfos of
        Left _ -> pure []
        Right driverInfos' -> pure $ QueriesDI.transformBeamDriverInformationToDomain <$> driverInfos'
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromLocations driverLocs

getOnRideStuckDriverIds :: Transactionable m => m [DriverInformation]
getOnRideStuckDriverIds = do
  Esq.findAll $ do
    driverInfos <- from $ table @DriverInformationT
    where_ $
      driverInfos ^. DriverInformationOnRide ==. val True
        &&. not_ (driverInfos ^. DriverInformationDriverId `in_` rideSubQuery)
    return driverInfos
  where
    rideSubQuery = subList_select $ do
      r <- from $ table @RideT
      where_ (r ^. RideStatus `in_` valList [Ride.INPROGRESS, Ride.NEW])
      pure (r ^. RideDriverId)

getVehicles ::
  L.MonadFlow m =>
  [DriverInformation] ->
  m [Vehicle]
getVehicles driverInfo = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      vehicles <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamV.driverId $ Se.In personKeys]
      case vehicles of
        Left _ -> pure []
        Right vehicles' -> pure $ QueriesV.transformBeamVehicleToDomain <$> vehicles'
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromInfo driverInfo

getDrivers ::
  (L.MonadFlow m, Log m) =>
  [Vehicle] ->
  m [Person]
getDrivers vehicles = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      persons <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamP.id $ Se.In personKeys]
      case persons of
        Left _ -> pure []
        Right persons' -> catMaybes <$> mapM transformBeamPersonToDomain persons'
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromVehicle vehicles

getDriversWithMerchID ::
  (L.MonadFlow m, Log m) =>
  Id Merchant ->
  m [Person]
getDriversWithMerchID (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      persons <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              ( [Se.Is BeamP.merchantId $ Se.Eq merchantId]
                  <> [Se.Is BeamP.role $ Se.Eq Person.DRIVER]
              )
          ]
      case persons of
        Left _ -> pure []
        Right persons' -> catMaybes <$> mapM transformBeamPersonToDomain persons'
    Nothing -> pure []

getDriverQuote ::
  (L.MonadFlow m, Log m) =>
  [Person] ->
  m [DriverQuote]
getDriverQuote persons = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      do
        res <-
          KV.findAllWithKVConnector
            dbConf'
            updatedMeshConfig
            [ Se.And [Se.Is BeamDQ.driverId $ Se.In personKeys, Se.Is BeamDQ.status $ Se.Eq DriverQuote.Active]
            ]
        case res of
          Right res' -> catMaybes <$> traverse QueriesDQ.transformBeamDriverQuoteToDomain res'
          _ -> pure []
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromPersons persons

getBookingInfo ::
  (L.MonadFlow m, Log m) =>
  [DriverQuote] ->
  m [Booking.Booking]
getBookingInfo driverQuote = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      res <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And [Se.Is BeamB.quoteId $ Se.In personsKeys, Se.Is BeamB.status $ Se.Eq Booking.TRIP_ASSIGNED]
          ]
      case res of
        Right res' -> do
          x <- mapM QB.transformBeamBookingToDomain res'
          pure $ catMaybes x
        _ -> pure []
    Nothing -> pure []
  where
    personsKeys = fetchDriverIDsTextFromQuote driverQuote

getBookingLocs ::
  L.MonadFlow m =>
  [Booking.Booking] ->
  m [BookingLocation]
getBookingLocs bookings = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      res <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamBL.id $ Se.In bookingKeys]
      case res of
        Left _ -> pure []
        Right res' -> pure $ QueriesBL.transformBeamBookingLocationToDomain <$> res'
    Nothing -> pure []
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

findAllDriverLocationWithSeConditions :: L.MonadFlow m => [Se.Clause Postgres BeamDL.DriverLocationT] -> m [DriverLocation]
findAllDriverLocationWithSeConditions conditions = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDL.DriverLocationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (QDL.transformBeamDriverLocationToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig conditions
    Nothing -> pure []

findAllDriverInformationWithSeConditions :: L.MonadFlow m => [Se.Clause Postgres BeamDI.DriverInformationT] -> m [DriverInformation]
findAllDriverInformationWithSeConditions conditions = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (QueriesDI.transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig conditions
    Nothing -> pure []

findAllVehiclesWithSeConditions :: L.MonadFlow m => [Se.Clause Postgres BeamV.VehicleT] -> m [Vehicle]
findAllVehiclesWithSeConditions conditions = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamV.VehicleT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (QV.transformBeamVehicleToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig conditions
    Nothing -> pure []

findAllBookingsWithSeConditions :: L.MonadFlow m => [Se.Clause Postgres BeamB.BookingT] -> m [Booking.Booking]
findAllBookingsWithSeConditions conditions = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamB.BookingT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      bookingL <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          conditions
      case bookingL of
        Right x -> catMaybes <$> traverse QB.transformBeamBookingToDomain x
        _ -> pure []
    Nothing -> pure []

findAllDriverQuoteWithSeConditions :: L.MonadFlow m => [Se.Clause Postgres BeamDQ.DriverQuoteT] -> m [DriverQuote]
findAllDriverQuoteWithSeConditions conditions = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDQ.DriverQuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      driverQuoteL <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          conditions
      case driverQuoteL of
        Right x -> catMaybes <$> traverse QDQ.transformBeamDriverQuoteToDomain x
        _ -> pure []
    Nothing -> pure []

findAllPersonWithSeConditionsNameAsc :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditionsNameAsc conditions = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      p <-
        KV.findAllWithOptionsKVConnector
          dbCOnf'
          updatedMeshConfig
          conditions
          (Se.Asc BeamP.firstName)
          Nothing
          Nothing
      case p of
        Right x -> catMaybes <$> traverse transformBeamPersonToDomain x
        _ -> pure []
    Nothing -> pure []

findAllPersonWithSeConditions :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditions conditions = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      p <-
        KV.findAllWithKVConnector
          dbCOnf'
          updatedMeshConfig
          conditions
      case p of
        Right x -> catMaybes <$> traverse transformBeamPersonToDomain x
        _ -> pure []
    Nothing -> pure []

findAllDriversByIdsFirstNameAsc' ::
  (L.MonadFlow m, Log m) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc' (Id merchantId) driverIds = do
  let personSeCondition =
        [ Se.And
            [ Se.Is BeamP.role $ Se.Eq Person.DRIVER,
              Se.Is BeamP.id $ Se.In $ getId <$> driverIds,
              Se.Is BeamP.merchantId $ Se.Eq merchantId
            ]
        ]
  personList <- findAllPersonWithSeConditionsNameAsc personSeCondition
  let dlSeCondition = [Se.Is BeamDL.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
  let infoSeCondition = [Se.Is BeamDI.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
  let vehicleSeCondition = [Se.Is BeamV.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
  dlList <- findAllDriverLocationWithSeConditions dlSeCondition
  infoList <- findAllDriverInformationWithSeConditions infoSeCondition
  vehicleList <- findAllVehiclesWithSeConditions vehicleSeCondition
  let pDl = foldl' (getPersonWithlocation dlList) [] personList
  let pDlInfo = foldl' (getPersonWithInfo infoList) [] pDl
  let pDlInfoVeh = foldl' (getPersonWithVehicle vehicleList) [] pDlInfo
  pure $ map mkFullDriver pDlInfoVeh
  where
    getPersonWithlocation dlList acc person' =
      let dlList' = filter (\dl -> dl.driverId == person'.id) dlList
       in acc <> ((\dl -> (person', dl)) <$> dlList')

    getPersonWithInfo infoList acc (person', dl') =
      let infoList' = filter (\info -> info.driverId == person'.id) infoList
       in acc <> ((\info -> (person', dl', info)) <$> infoList')

    getPersonWithVehicle vehicleList acc (person', dl', info') =
      let vehicleList' = filter (\vehicle -> vehicle.driverId == person'.id) vehicleList
       in acc <> ((\vehicle -> (person', dl', info', vehicle)) <$> vehicleList')

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int
  }

mkDriverWithRidesCount :: (Person, DriverInformation, Maybe Vehicle, Maybe Int) -> DriverWithRidesCount
mkDriverWithRidesCount (person, info, vehicle, ridesCount) = DriverWithRidesCount {..}

fetchDriverInfoWithRidesCount :: (Transactionable m, MonadTime m) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe DriverWithRidesCount)
fetchDriverInfoWithRidesCount merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  mbDriverInfo <- fetchDriverInfo merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash
  addRidesCount `mapM` mbDriverInfo
  where
    addRidesCount :: Transactionable m => (Person, DriverInformation, Maybe Vehicle) -> m DriverWithRidesCount
    addRidesCount (person, info, vehicle) = do
      ridesCount <-
        Esq.findOne $ do
          ride <- from $ table @RideT
          where_ $
            ride ^. RideDriverId ==. val (toKey person.id)
              &&. not_ (ride ^. RideStatus `in_` valList [Ride.NEW, Ride.CANCELLED])
          groupBy $ ride ^. RideDriverId
          return (count @Int $ ride ^. RideId)
      return $ mkDriverWithRidesCount (person, info, vehicle, ridesCount)

fetchDriverInfo :: (Transactionable m, MonadTime m) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe (Person, DriverInformation, Maybe Vehicle))
fetchDriverInfo merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  now <- getCurrentTime
  Esq.findOne $ do
    person :& driverInfo :& mbVehicle :& mbDriverLicense :& _mbRcAssoc :& mbRegCert <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
          `Esq.on` ( \(person :& driverInfo) ->
                       person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
                   )
          `leftJoin` table @VehicleT
          `Esq.on` ( \(person :& _ :& mbVehicle) ->
                       just (person ^. PersonTId) ==. mbVehicle ?. VehicleDriverId
                   )
          `leftJoin` table @DriverLicenseT
          `Esq.on` ( \(person :& _ :& _ :& mbDriverLicense) ->
                       joinOnlyWhenJust mbDlNumberHash $ just (person ^. PersonTId) ==. mbDriverLicense ?. DriverLicenseDriverId
                   )
          `leftJoin` table @DriverRCAssociationT
          `Esq.on` ( \(person :& _ :& _ :& _ :& mbRcAssoc) ->
                       joinOnlyWhenJust mbRcNumberHash $
                         do
                           just (person ^. PersonTId) ==. mbRcAssoc ?. DriverRCAssociationDriverId
                           &&. just (just (val now)) <. mbRcAssoc ?. DriverRCAssociationAssociatedTill
                   )
          `leftJoin` table @VehicleRegistrationCertificateT
          `Esq.on` ( \(_ :& _ :& _ :& _ :& mbRcAssoc :& mbRegCert) ->
                       joinOnlyWhenJust mbRcNumberHash $
                         mbRcAssoc ?. DriverRCAssociationRcId ==. mbRegCert ?. VehicleRegistrationCertificateTId
                   )
    where_ $
      person ^. PersonMerchantId ==. (val . toKey $ merchantId)
        &&. person ^. PersonRole ==. val Person.DRIVER
        &&. whenJust_
          mbMobileNumberDbHashWithCode
          ( \(mobileNumberDbHash, mobileCountryCode) ->
              person ^. PersonMobileCountryCode ==. val (Just mobileCountryCode)
                &&. ( person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
                        ||. person ^. PersonAlternateMobileNumberHash ==. val (Just mobileNumberDbHash)
                    )
          )
        &&. whenJust_ mbVehicleNumber (\vehicleNumber -> mbVehicle ?. VehicleRegistrationNo ==. just (val vehicleNumber))
        &&. whenJust_ mbDlNumberHash (\dlNumberHash -> mbDriverLicense ?. DriverLicenseLicenseNumberHash ==. just (val dlNumberHash))
        &&. whenJust_ mbRcNumberHash (\rcNumberHash -> mbRegCert ?. VehicleRegistrationCertificateCertificateNumberHash ==. just (val rcNumberHash))
    pure (person, driverInfo, mbVehicle)
  where
    joinOnlyWhenJust mbFilter cond = maybe (val False) (const cond) mbFilter

findByIdAndRoleAndMerchantId :: (L.MonadFlow m, Log m) => Id Person -> Person.Role -> Id Merchant -> m (Maybe Person)
findByIdAndRoleAndMerchantId (Id pid) role_ (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamP.id $ Se.Eq pid, Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

findAllByMerchantId :: (L.MonadFlow m, Log m) => [Person.Role] -> Id Merchant -> m [Person]
findAllByMerchantId roles (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.In roles]]
      case result of
        Right p -> catMaybes <$> traverse transformBeamPersonToDomain p
        _ -> pure []
    Nothing -> pure []

findAdminsByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m [Person]
findAdminsByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.Eq Person.ADMIN]]
      case result of
        Right p -> catMaybes <$> traverse transformBeamPersonToDomain p
        _ -> pure []
    Nothing -> pure []

findByMobileNumberAndMerchant :: (L.MonadFlow m, Log m) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchant countryCode mobileNumberHash (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
                Se.Is BeamP.merchantId $ Se.Eq merchantId,
                Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash]
              ]
          ]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

findByIdentifierAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByIdentifierAndMerchant (Id merchantId) identifier_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamP.identifier $ Se.Eq $ Just identifier_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

findByEmailAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByEmailAndMerchant (Id merchantId) email_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamP.email $ Se.Eq $ Just email_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

findByRoleAndMobileNumberAndMerchantId :: (L.MonadFlow m, Log m, EncFlow m r) => Role -> Text -> Text -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber_ (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  mobileNumberDbHash <- getDbHash mobileNumber_
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamP.role $ Se.Eq role_,
                Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
                Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberDbHash,
                Se.Is BeamP.merchantId $ Se.Eq merchantId
              ]
          ]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

personDriverTable ::
  From
    ( Table PersonT
        :& Table DriverInformationT
    )
personDriverTable =
  table @PersonT
    `innerJoin` table @DriverInformationT
    `Esq.on` ( \(person :& driver) ->
                 person ^. PersonTId ==. driver ^. DriverInformationDriverId
                   &&. Esq.not_ (driver ^. DriverInformationBlocked)
             )

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

updateMerchantIdAndMakeAdmin :: (L.MonadFlow m, MonadTime m) => Id Person -> Id Merchant -> m (MeshResult ())
updateMerchantIdAndMakeAdmin (Id personId) (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamP.merchantId merchantId,
          Se.Set BeamP.role Person.ADMIN,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateName :: (L.MonadFlow m, MonadTime m) => Id Person -> Text -> m (MeshResult ())
updateName (Id personId) name = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamP.firstName name,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updatePersonRec :: (L.MonadFlow m, MonadTime m) => Id Person -> Person -> m (MeshResult ())
updatePersonRec (Id personId) person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamP.firstName $ person.firstName,
          Se.Set BeamP.middleName $ person.middleName,
          Se.Set BeamP.lastName $ person.lastName,
          Se.Set BeamP.gender $ person.gender,
          Se.Set BeamP.email $ person.email,
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
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updatePersonVersions :: (L.MonadFlow m, MonadTime m) => Person -> Maybe Version -> Maybe Version -> m ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      dbConf <- L.getOption KBT.PsqlDbCfg
      let modelName = Se.modelTableName @BeamP.PersonT
      let updatedMeshConfig = setMeshConfig modelName
      case dbConf of
        Just dbConf' ->
          void $
            KV.updateWoReturningWithKVConnector
              dbConf'
              updatedMeshConfig
              [ Se.Set BeamP.clientVersion mbClientVersionText,
                Se.Set BeamP.bundleVersion mbBundleVersionText,
                Se.Set BeamP.updatedAt now
              ]
              [Se.Is BeamP.id (Se.Eq $ getId person.id)]
        Nothing -> pure ()

updateDeviceToken :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe FCMRecipientToken -> m (MeshResult ())
updateDeviceToken (Id personId) mbDeviceToken = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamP.deviceToken mbDeviceToken,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateWhatsappNotificationEnrollStatus :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe Whatsapp.OptApiMethods -> m ()
updateWhatsappNotificationEnrollStatus (Id personId) enrollStatus = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamP.whatsappNotificationEnrollStatus enrollStatus,
            Se.Set BeamP.updatedAt now
          ]
          [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure ()

updateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m, Log m, EncFlow m r) => Person -> m (MeshResult ())
updateMobileNumberAndCode person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
          Se.Set BeamP.mobileNumberEncrypted $ person.mobileNumber <&> unEncrypted . (.encrypted),
          Se.Set BeamP.mobileNumberHash $ person.mobileNumber <&> (.hash),
          Se.Set BeamP.unencryptedMobileNumber $ person.unencryptedMobileNumber,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq $ getId person.id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

setIsNewFalse :: (L.MonadFlow m, MonadTime m) => Id Person -> m ()
setIsNewFalse (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamP.isNew False,
            Se.Set BeamP.updatedAt now
          ]
          [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure ()

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure ()

updateAverageRating :: (L.MonadFlow m, MonadTime m) => Id Person -> Centesimal -> m (MeshResult ())
updateAverageRating (Id personId) newAverageRating = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamP.rating (Just newAverageRating),
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

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
  (Transactionable m, MonadTime m, L.MonadFlow m) =>
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
  (L.MonadFlow m, MonadTime m) =>
  Id Merchant ->
  Maybe Seconds ->
  LatLong ->
  Int ->
  m [DriverLocation]
getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry LatLong {..} radiusMeters = QueriesDL.getDriverLocsFromMerchId mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId

getDriverInfosWithCond :: L.MonadFlow m => [DriverLocation] -> Bool -> Bool -> m [DriverInformation]
getDriverInfosWithCond driverLocs onlyNotOnRide onlyOnRide = do
  dbCOnf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
      updatedMeshConfig = setMeshConfig modelName
  case dbCOnf of
    Just dbCOnf' -> do
      either (pure []) (QueriesDI.transformBeamDriverInformationToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          updatedMeshConfig
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
    Nothing -> pure []
  where
    personsKeys = getId . cast <$> fetchDriverIDsFromLocations driverLocs

getVehiclesWithCond ::
  L.MonadFlow m =>
  [DriverInformation] ->
  m [Vehicle]
getVehiclesWithCond driverInfo = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      vehicles <- KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamV.driverId $ Se.In personsKeys]
      case vehicles of
        Right val' -> pure $ QueriesV.transformBeamVehicleToDomain <$> val'
        Left _ -> pure []
    Nothing -> pure []
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
  (Transactionable m, MonadTime m, L.MonadFlow m) =>
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

linkArrayListForOnRide :: [DriverQuote] -> [Booking.Booking] -> [BookingLocation] -> [DriverLocation] -> [DriverInformation] -> [Vehicle] -> [Person] -> LatLong -> Double -> Maybe Variant -> [(Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Variant, Double, Double, Double, Double, Maybe DriverInfo.DriverMode)]
linkArrayListForOnRide driverQuotes bookings bookingLocs driverLocations driverInformations vehicles persons LatLong {..} onRideRadius mbVariant =
  let locationHashMap = buildLocationHashMap driverLocations
      personHashMap = buildPersonHashMap persons
      quotesHashMap = buildQuotesHashMap driverQuotes
      bookingHashMap = buildBookingHashMap bookings
      bookingLocsHashMap = buildBookingLocsHashMap bookingLocs
      driverInfoHashMap = buildDriverInfoHashMap driverInformations
   in mapMaybe (buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap LatLong {..} onRideRadius mbVariant) vehicles

getNearestDriversCurrentlyOnRide' :: (L.MonadFlow m, Log m, MonadTime m) => Maybe Variant -> Int -> Id Merchant -> Maybe Seconds -> Int -> m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide' mbVariant radiusMeters (Id merchantId') mbDriverPositionInfoExpiry reduceRadiusValue = do
  now <- getCurrentTime

  let onRideRadius = (fromIntegral (radiusMeters - reduceRadiusValue) :: Double)
  let personSeCondition =
        [ Se.And
            [ Se.Is BeamP.role $ Se.Eq Person.DRIVER,
              Se.Is BeamP.merchantId $ Se.Eq merchantId'
            ]
        ]
  personList <- findAllPersonWithSeConditionsNameAsc personSeCondition
  dlList <- QueriesDL.findAllDriverLocations ((Person.id :: PersonE e -> Id Person) <$> personList) mbDriverPositionInfoExpiry now

  let diConditions =
        [ Se.And
            [ Se.Is BeamDI.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList,
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
              Se.Is BeamDI.onRide $ Se.Eq True,
              Se.Is BeamDI.blocked $ Se.Eq False
            ]
        ]
  driverInfoList <- findAllDriverInformationWithSeConditions diConditions

  let vehicleSeCondition = [Se.Is BeamV.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
  vehicleList <- findAllVehiclesWithSeConditions vehicleSeCondition

  let dqSeConditions = [Se.Is BeamDQ.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
  driverQuoteList <- findAllDriverQuoteWithSeConditions dqSeConditions

  let bookingSeConditions =
        [ Se.And
            [ Se.Is BeamB.quoteId $ Se.In $ getId . DriverQuote.id <$> driverQuoteList,
              Se.Is BeamB.status $ Se.Eq Booking.TRIP_ASSIGNED
            ]
        ]
  bookingList <- findAllBookingsWithSeConditions bookingSeConditions

  let pDl = foldl' (getPersonWithlocation dlList) [] personList
  let pDlInfo = foldl' (getPersonWithInfo driverInfoList) [] pDl
  let pDlInfoVeh = foldl' (getPersonDInfoDLVehicle mbVariant vehicleList) [] pDlInfo
  let pDlInfoVehQuote = foldl' (getPersonDInfoDLVehQuote driverQuoteList) [] pDlInfoVeh
  let pDlInfoVehQuoteBooking = foldl' (getPersonDInfoDLVehQuoteBooking bookingList) [] pDlInfoVehQuote
  let pDlInfoVehQuoteBooking' = map (\(person', dl', info', vehicle', _, booking') -> (person'.id, person'.deviceToken, person'.language, info'.onRide, info'.canDowngradeToSedan, info'.canDowngradeToHatchback, info'.canDowngradeToTaxi, dl'.lat, dl'.lon, vehicle'.variant, booking'.toLocation.lat, booking'.toLocation.lat, onRideRadius, onRideRadius, info'.mode)) pDlInfoVehQuoteBooking
  pure $ makeNearestDriversResult =<< pDlInfoVehQuoteBooking'
  where
    getPersonWithlocation dlList acc person' =
      let dlList' = filter (\dl -> dl.driverId == person'.id) dlList
       in acc <> ((\dl -> (person', dl)) <$> dlList')

    getPersonWithInfo driverInfoList acc (person', dl') =
      let driverInfoList' = filter (\info -> info.driverId == person'.id) driverInfoList
       in acc <> ((\info -> (person', dl', info)) <$> driverInfoList')

    getPersonDInfoDLVehicle mbVariant' vehicleList acc (person', dl', info') =
      let vehicleList' =
            filter
              ( \vehicle ->
                  ( Kernel.Prelude.isNothing mbVariant'
                      || ( Kernel.Prelude.isNothing mbVariant
                             || ( Just vehicle.variant == mbVariant'
                                    || ( case mbVariant' of
                                           Just SEDAN -> info'.canDowngradeToSedan && vehicle.variant == SUV
                                           Just HATCHBACK -> info'.canDowngradeToHatchback && (vehicle.variant == SEDAN || vehicle.variant == SUV)
                                           Just TAXI -> info'.canDowngradeToTaxi && vehicle.variant == TAXI_PLUS
                                           _ -> False
                                       )
                                )
                         )
                  )
                    && (vehicle.driverId == person'.id)
              )
              vehicleList
       in acc <> ((\vehicle -> (person', dl', info', vehicle)) <$> vehicleList')

    getPersonDInfoDLVehQuote driverQuoteList acc (person', dl', info', vehicle') =
      let driverQuoteList' = filter (\quote -> quote.driverId == person'.id) driverQuoteList
       in acc <> ((\quote -> (person', dl', info', vehicle', quote)) <$> driverQuoteList')

    getPersonDInfoDLVehQuoteBooking bookingList acc (person', dl', info', vehicle', quote') =
      let bookingList' = filter (\booking -> booking.quoteId == getId quote'.id) bookingList
       in acc <> ((\booking -> (person', dl', info', vehicle', quote', booking)) <$> bookingList')

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

updateAlternateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m) => Person -> m (MeshResult ())
updateAlternateMobileNumberAndCode person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamP.PersonT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamP.alternateMobileNumberEncrypted (person.alternateMobileNumber <&> unEncrypted . (.encrypted)),
          Se.Set BeamP.unencryptedAlternateMobileNumber person.unencryptedAlternateMobileNumber,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq $ getId person.id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamPersonToDomain :: (L.MonadFlow m, Log m) => BeamP.Person -> m (Maybe Person)
transformBeamPersonToDomain BeamP.PersonT {..} = do
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
          identifierType = identifierType,
          email = email,
          unencryptedMobileNumber = unencryptedMobileNumber,
          mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
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

transformDomainPersonToBeam :: Person -> BeamP.Person
transformDomainPersonToBeam Person {..} =
  BeamP.PersonT
    { BeamP.id = getId id,
      BeamP.firstName = firstName,
      BeamP.middleName = middleName,
      BeamP.lastName = lastName,
      BeamP.role = role,
      BeamP.gender = gender,
      BeamP.identifierType = identifierType,
      BeamP.email = email,
      BeamP.unencryptedMobileNumber = unencryptedMobileNumber,
      BeamP.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
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
