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
import qualified Database.Beam as B
import Database.Beam.Postgres hiding ((++.))
import qualified Database.Beam.Query ()
-- import Domain.Type.Booking

-- import qualified Lib.Utils as Utils

-- import qualified Storage.Beam.DriverLocation as BeamDL

-- import Domain.Types.DriverQuote as DDQ

-- import qualified Storage.Queries.FareParameters as QueriesFP
-- import qualified Storage.Beam.FareParameters as BeamFP

-- import qualified Kernel.Prelude

import qualified Debug.Trace as T (trace, traceShowId)
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
import EulerHS.KVConnector.Utils (meshModelTableEntity)
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
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
-- import Servant
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Booking.BookingLocation as BeamBL
import qualified Storage.Beam.DriverInformation as BeamDI
-- import qualified Kernel.Prelude
import qualified Storage.Beam.DriverLocation as BeamDL
import qualified Storage.Beam.DriverQuote as BeamDQ
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Vehicle as BeamV
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking as QueriesB
import qualified Storage.Queries.Booking.BookingLocation as QueriesBL
import qualified Storage.Queries.DriverInformation as QueriesDI
import qualified Storage.Queries.DriverLocation as QDL
import qualified Storage.Queries.DriverLocation as QueriesDL
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverQuote as QueriesDQ
import Storage.Queries.FullEntityBuilders
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.Vehicle as QueriesV
import Storage.Tabular.Booking
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.DriverQuote
import Storage.Tabular.Person as TPerson
import Storage.Tabular.Ride
import Storage.Tabular.Vehicle as Vehicle

-- import Lib.Utils as Utils

-- create :: Person -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => Person.Person -> m (MeshResult BeamP.Person)
create person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> T.trace "Rahul" (T.traceShowId <$> KV.createWithKVConnector dbConf' Mesh.meshConfig (transformDomainPersonToBeam person))
    Nothing -> T.trace "RahulNothing" pure (Left $ MKeyNotFound "DB Config not found")

-- findById ::
--   Transactionable m =>
--   Id Person ->
--   m (Maybe Person)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Person)
findById (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamP.id $ Se.Eq personId]
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

-- countDrivers :: Transactionable m => Id Merchant -> m Int
-- countDrivers merchantId =
--   mkCount <$> do
--     Esq.findAll $ do
--       person <- from $ table @PersonT
--       where_ $
--         person ^. PersonMerchantId ==. val (toKey merchantId)
--           &&. person ^. PersonRole ==. val Person.DRIVER
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0

getDriversList ::
  (L.MonadFlow m, Log m) =>
  [DriverInformation] ->
  m [Person]
getDriversList driverInfos = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamP.id $ Se.In personsKeys]
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
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findAllWithKVConnector
          dbConf'
          Mesh.meshConfig
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
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamDL.updatedAt $ Se.LessThan before]
      case result of
        Left _ -> pure []
        Right result' -> pure $ QueriesDL.transformBeamDriverLocationToDomain <$> result'
    Nothing -> pure []

getDriversWithOutdatedLocationsToMakeInactive :: (L.MonadFlow m, Log m) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before = do
  driverLocations <- getDriverLocations before
  driverInfos <- getDriverInformations driverLocations
  getDriversList driverInfos

findAllDriversByIdsFirstNameAsc ::
  (Transactionable m, Functor m) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc merchantId driverIds = do
  driverLocs <- getDriverLocs driverIds merchantId
  driverInfos <- getDriverInfos driverLocs
  vehicle <- getVehicles driverInfos
  drivers <- getDrivers vehicle
  return (linkArrays driverLocs driverInfos vehicle drivers)

linkArrays :: [DriverLocation] -> [DriverInformation] -> [Vehicle] -> [Person] -> [FullDriver]
linkArrays driverLocations driverInformations vehicles persons =
  let locationHashMap = buildLocationHashMap driverLocations
      vehicleHashMap = buildVehicleHashMap vehicles
      driverInfoHashMap = buildDriverInfoHashMap driverInformations
   in map (buildFullDriver locationHashMap vehicleHashMap driverInfoHashMap) persons

linkArrayList :: [DriverLocation] -> [DriverInformation] -> [Vehicle] -> [Person] -> LatLong -> Maybe Variant -> [(Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode)]
linkArrayList driverLocations driverInformations vehicles persons LatLong {..} mbVariant =
  let locationHashMap = buildLocationHashMap driverLocations
      driverInfoHashMap = buildDriverInfoHashMap driverInformations
      vehicleHashMap = buildVehicleHashMap vehicles
   in mapMaybe (buildFullDriverList locationHashMap vehicleHashMap driverInfoHashMap LatLong {..} mbVariant) persons

buildLocationHashMap :: [DriverLocation] -> HashMap.HashMap Text DriverLocation
buildLocationHashMap driverLocations =
  HashMap.fromList $ map (\loc -> (loc.driverId.getId, loc)) driverLocations

buildPersonHashMap :: [Person] -> HashMap.HashMap Text Person
buildPersonHashMap persons =
  HashMap.fromList $ map (\p -> (p.id.getId, p)) persons

buildVehicleHashMap :: [Vehicle] -> HashMap.HashMap Text Vehicle
buildVehicleHashMap vehicles =
  HashMap.fromList $ map (\v -> (v.driverId.getId, v)) vehicles

buildFullDriver :: HashMap.HashMap Text DriverLocation -> HashMap.HashMap Text Vehicle -> HashMap.HashMap Text DriverInformation -> Person -> FullDriver
buildFullDriver locationHashMap vehicleHashMap driverInfoHashMap person =
  let driverId' = person.id.getId
      location = HashMap.lookupDefault (error "Location not found") driverId' locationHashMap
      vehicle = HashMap.lookupDefault (error "Vehicle not found") driverId' vehicleHashMap
      info = HashMap.lookupDefault (error "Person not found") driverId' driverInfoHashMap
   in mkFullDriver (person, location, info, vehicle)

getDriverLocs ::
  Transactionable m =>
  [Id Person] ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocs driverIds merchantId = do
  Esq.findAll $ do
    driverLocs <- from $ table @DriverLocationT
    where_ $
      driverLocs ^. DriverLocationDriverId `in_` valList personsKeys
        &&. driverLocs ^. DriverLocationMerchantId ==. (val . toKey $ merchantId)
    return driverLocs
  where
    personsKeys = toKey . cast <$> driverIds

getDriverLocs' ::
  L.MonadFlow m =>
  [Id Person] ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocs' driverIds (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      driverLocs <-
        KV.findAllWithKVConnector
          dbConf'
          Mesh.meshConfig
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
  Transactionable m =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos' driverLocs = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      driverInfos <- KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamDI.driverId $ Se.In personKeys]
      case driverInfos of
        Left _ -> pure []
        Right driverInfos' -> pure $ QueriesDI.transformBeamDriverInformationToDomain <$> driverInfos'
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromLocations driverLocs

getVehicles ::
  Transactionable m =>
  [DriverInformation] ->
  m [Vehicle]
getVehicles' driverInfo = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      vehicles <- KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamV.driverId $ Se.In personKeys]
      case vehicles of
        Left _ -> pure []
        Right vehicles' -> pure $ QueriesV.transformBeamVehicleToDomain <$> vehicles'
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromInfo driverInfo

getDrivers ::
  Transactionable m =>
  [Vehicle] ->
  m [Person]
getDrivers' vehicles = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      persons <- KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamP.id $ Se.In personKeys]
      case persons of
        Left _ -> pure []
        Right persons' -> catMaybes <$> mapM transformBeamPersonToDomain persons'
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromVehicle vehicles

getDriversWithMerchID ::
  Transactionable m =>
  Id Merchant ->
  m [Person]
getDriversWithMerchID merchantId = do
  Esq.findAll $ do
    persons <- from $ table @PersonT
    where_ $
      persons ^. PersonMerchantId ==. val (toKey merchantId)
        &&. persons ^. PersonRole ==. val Person.DRIVER
    return persons

getDriversWithMerchID' ::
  (L.MonadFlow m, Log m) =>
  Id Merchant ->
  m [Person]
getDriversWithMerchID' (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      persons <-
        KV.findAllWithKVConnector
          dbConf'
          Mesh.meshConfig
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
  Transactionable m =>
  [Person] ->
  m [DriverQuote]
getDriverQuote persons =
  buildDType $ do
    res <- Esq.findAll' $ do
      (dQuote :& farePars) <-
        from QDQ.baseDriverQuoteQuery
      where_ $
        dQuote ^. DriverQuoteDriverId `in_` valList personsKeys
          &&. dQuote ^. DriverQuoteStatus ==. val DriverQuote.Active
      pure (dQuote, farePars)
    catMaybes <$> mapM buildFullDriverQuote res
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromPersons persons

getDriverQuote' ::
  (L.MonadFlow m, Log m) =>
  [Person] ->
  m [DriverQuote]
getDriverQuote' persons = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      do
        res <-
          KV.findAllWithKVConnector
            dbConf'
            Mesh.meshConfig
            [ Se.And [Se.Is BeamDQ.driverId $ Se.In personKeys, Se.Is BeamDQ.status $ Se.Eq DriverQuote.Active]
            ]
        case res of
          Right res' -> catMaybes <$> traverse QueriesDQ.transformBeamDriverQuoteToDomain res'
          _ -> pure []

    -- fareParam <- do
    --   res <- KV.findAllWithKVConnector
    --       dbConf'
    --       Mesh.meshConfig
    --       [ Se.And [Se.Is BeamFP.id $ Se.In $ getId . DDQ.fareParams.id <$> driverQuotes ]
    --       ]
    --   case res of
    --     Left _ -> pure []
    --     Right res' -> traverse QueriesFP.transformBeamFareParametersToDomain res'
    -- let dqWithFP = foldl' (getDriverQuoteWithFareParam fareParam) [] driverQuotes
    -- pure dqWithFP
    Nothing -> pure []
  where
    personKeys = getId <$> fetchDriverIDsFromPersons persons

-- getDriverQuoteWithFareParam fareParams acc dq' =
--   let fareParams' = filter(\x -> x.id == dq'.fareParams.id) fareParams
--    in acc <> dq' <$> fareParams'

getBookingInfo ::
  Transactionable m =>
  [DriverQuote] ->
  m [Booking.Booking]
getBookingInfo driverQuote = buildDType $ do
  res <-
    Esq.findAll' $ do
      booking <- from $ table @BookingT
      where_ $
        booking ^. BookingQuoteId `in_` valList personsKeys
          &&. booking ^. BookingStatus ==. val Booking.TRIP_ASSIGNED
      return booking
  catMaybes <$> mapM buildFullBooking res
  where
    personsKeys = fetchDriverIDsTextFromQuote driverQuote

getBookingInfo' ::
  (L.MonadFlow m, Log m) =>
  [DriverQuote] ->
  m [Booking.Booking]
getBookingInfo' driverQuote = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      res <-
        KV.findAllWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.And [Se.Is BeamB.quoteId $ Se.In personsKeys, Se.Is BeamB.status $ Se.Eq Booking.TRIP_ASSIGNED]
          ]
      case res of
        Right res' -> do
          x <- mapM QueriesB.transformBeamBookingToDomain res'
          pure $ catMaybes x
        _ -> pure []
    Nothing -> pure []
  where
    personsKeys = fetchDriverIDsTextFromQuote driverQuote

getBookingLocs ::
  Transactionable m =>
  [Booking.Booking] ->
  m [BookingLocation]
getBookingLocs' bookings = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      res <-
        KV.findAllWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamBL.id $ Se.In bookingKeys]
      case res of
        Left _ -> pure []
        Right res' -> pure $ QueriesBL.transformBeamBookingLocationToDomain <$> res'
    Nothing -> pure []
  where
    bookingKeys = getId <$> fetchToLocationIDFromBooking bookings

getDriverLocsFromMerchId ::
  (Transactionable m, MonadTime m) =>
  Maybe Seconds ->
  LatLong ->
  Int ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocsFromMerchId mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId = do
  now <- getCurrentTime
  Esq.findAll $ do
    driverLoc <- from $ table @DriverLocationT
    where_ $
      driverLoc ^. DriverLocationMerchantId ==. (val . toKey $ merchantId)
        &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                ||. (driverLoc ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
            )
        &&. buildRadiusWithin (driverLoc ^. DriverLocationPoint) (lat, lon) (val radiusMeters)
    orderBy [asc (driverLoc ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon))]
    return driverLoc

-- getDriverLocsFromMerchId' :: (L.MonadFlow m, Log m, MonadTime m) =>
--   Maybe Seconds ->
--   LatLong ->
--   Int ->
--   Id Merchant ->
--   m [DriverLocation]
-- getDriverLocsFromMerchId' mbDriverPositionInfoExpiry LatLong {..} radiusMeters (Id merchantId) = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   case dbConf of
--     Just dbConf' -> do
--       res <- KV.findAllWithKVConnector
--           dbConf'
--           Mesh.meshConfig
--           [ Se.And [Se.Is BeamDL.merchantId $ Se.Eq  merchantId,Se.Is Utils.buildRadiusWithin' (lat, lon) (radiusMeters)]
--             ]
--       case res of
--         Left _ -> pure []
--         Right res' -> pure $ QueriesDL.transformBeamDriverLocationToDomain<$> res'
--     Nothing -> pure []

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

findAllDriversByIdsFirstNameAsc' ::
  (L.MonadFlow m, Log m) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc' (Id merchantId) driverIds = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      personList <- do
        p <-
          KV.findAllWithOptionsKVConnector
            dbCOnf'
            Mesh.meshConfig
            [ Se.And
                [ Se.Is BeamP.role $ Se.Eq Person.DRIVER,
                  Se.Is BeamP.id $ Se.In $ getId <$> driverIds,
                  Se.Is BeamP.merchantId $ Se.Eq merchantId
                ]
            ]
            (Se.Asc BeamP.firstName)
            Nothing
            Nothing
        case p of
          Right x -> catMaybes <$> traverse transformBeamPersonToDomain x
          _ -> pure []
      -- either (pure []) (transformBeamPersonToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamP.role $ Se.Eq Person.DRIVER,
      --               Se.Is BeamP.id $ Se.In $ getId <$> driverIds, Se.Is BeamP.merchantId $ Se.Eq merchantId ]] (Se.Asc BeamP.firstName) Nothing Nothing
      dlList <- either (pure []) (QDL.transformBeamDriverLocationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDL.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
      infoList <- either (pure []) (QueriesDI.transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDI.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
      vehicleList <- either (pure []) (QV.transformBeamVehicleToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamV.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
      let pDl = foldl' (getPersonWithlocation dlList) [] personList
      let pDlInfo = foldl' (getPersonWithInfo infoList) [] pDl
      let pDlInfoVeh = foldl' (getPersonWithVehicle vehicleList) [] pDlInfo
      pure $ map mkFullDriver pDlInfoVeh
    Nothing -> pure []
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

-- lets not use this as of now
-- findAllDriversByIdsFirstNameAsc' ::
--   (L.MonadFlow m, Log m) =>
--   Id Merchant ->
--   [Id Person] ->
--   m [FullDriver]
-- findAllDriversByIdsFirstNameAsc' (Id merchantId) driverIds = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   case dbConf of
--     Just dbCOnf' -> do
--       personList <- do
--         p <-
--           KV.findAllWithOptionsKVConnector
--             dbCOnf'
--             Mesh.meshConfig
--             [ Se.And
--                 [ Se.Is BeamP.role $ Se.Eq Person.DRIVER,
--                   Se.Is BeamP.id $ Se.In $ getId <$> driverIds,
--                   Se.Is BeamP.merchantId $ Se.Eq merchantId
--                 ]
--             ]
--             (Se.Asc BeamP.firstName)
--             Nothing
--             Nothing
--         case p of
--           Left _ -> pure []
--           Right x -> traverse transformBeamPersonToDomain x
--       -- either (pure []) (transformBeamPersonToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamP.role $ Se.Eq Person.DRIVER,
--       --               Se.Is BeamP.id $ Se.In $ getId <$> driverIds, Se.Is BeamP.merchantId $ Se.Eq merchantId ]] (Se.Asc BeamP.firstName) Nothing Nothing
--       dlList <- findAllDriverLocations' dbCOnf' (getId . (Person.id :: PersonE e -> Id Person) <$> personList)
--       infoList <- either (pure []) (QueriesDI.transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDI.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
--       vehicleList <- either (pure []) (QV.transformBeamVehicleToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamV.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
--       let pDl = foldl' (getPersonWithlocation dlList) [] personList
--       let pDlInfo = foldl' (getPersonWithInfo infoList) [] pDl
--       let pDlInfoVeh = foldl' (getPersonWithVehicle vehicleList) [] pDlInfo
--       pure $ map mkFullDriver pDlInfoVeh
--     Nothing -> pure []
--   where
--     getPersonWithlocation dlList acc person' =
--       let dlList' = filter (\dl -> dl.driverId == person'.id) dlList
--        in acc <> ((\dl -> (person', dl)) <$> dlList')

--     getPersonWithInfo infoList acc (person', dl') =
--       let infoList' = filter (\info -> info.driverId == person'.id) infoList
--        in acc <> ((\info -> (person', dl', info)) <$> infoList')

--     getPersonWithVehicle vehicleList acc (person', dl', info') =
--       let vehicleList' = filter (\vehicle -> vehicle.driverId == person'.id) vehicleList
--        in acc <> ((\vehicle -> (person', dl', info', vehicle)) <$> vehicleList')

--     findAllDriverLocations' dbCOnf' driverIdList = do
--       conn <- L.getOrInitSqlConn dbCOnf'
--       case conn of
--         Right c -> do
--           geoms <-
--             L.runDB c $
--               L.findRows $
--                 B.select $
--                   B.filter_' (\BeamDL.DriverLocationT {..} -> B.sqlBool_ (driverId `B.in_` (B.val_ <$> driverIdList))) $
--                     B.all_ (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
--           pure (either (const []) (QDL.transformBeamDriverLocationToDomain <$>) geoms)
--         Left _ -> pure []

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
                &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
          )
        &&. whenJust_ mbVehicleNumber (\vehicleNumber -> mbVehicle ?. VehicleRegistrationNo ==. just (val vehicleNumber))
        &&. whenJust_ mbDlNumberHash (\dlNumberHash -> mbDriverLicense ?. DriverLicenseLicenseNumberHash ==. just (val dlNumberHash))
        &&. whenJust_ mbRcNumberHash (\rcNumberHash -> mbRegCert ?. VehicleRegistrationCertificateCertificateNumberHash ==. just (val rcNumberHash))
    pure (person, driverInfo, mbVehicle)
  where
    -- used only for dl and rc entites, because they are not required for final result, only for filters
    joinOnlyWhenJust mbFilter cond = maybe (val False) (const cond) mbFilter

-- findByIdAndRoleAndMerchantId ::
--   Transactionable m =>
--   Id Person ->
--   Person.Role ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByIdAndRoleAndMerchantId pid role_ merchantId =
--   Esq.findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonTId ==. val (toKey pid)
--         &&. person ^. PersonRole ==. val role_
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByIdAndRoleAndMerchantId :: (L.MonadFlow m, Log m) => Id Person -> Person.Role -> Id Merchant -> m (Maybe Person)
findByIdAndRoleAndMerchantId (Id pid) role_ (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamP.id $ Se.Eq pid, Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

-- findAllByMerchantId ::
--   Transactionable m =>
--   [Person.Role] ->
--   Id Merchant ->
--   m [Person]
-- findAllByMerchantId roles merchantId =
--   Esq.findAll $ do
--     person <- from $ table @PersonT
--     where_ $
--       (person ^. PersonRole `in_` valList roles ||. val (null roles))
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findAllByMerchantId :: (L.MonadFlow m, Log m) => [Person.Role] -> Id Merchant -> m [Person]
findAllByMerchantId roles (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.In roles]]
      case result of
        Right p -> catMaybes <$> traverse transformBeamPersonToDomain p
        _ -> pure []
    Nothing -> pure []

-- findAdminsByMerchantId :: Transactionable m => Id Merchant -> m [Person]
-- findAdminsByMerchantId merchantId =
--   Esq.findAll $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonMerchantId ==. val (toKey merchantId)
--         &&. person ^. PersonRole ==. val Person.ADMIN
--     return person

findAdminsByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m [Person]
findAdminsByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.Eq Person.ADMIN]]
      case result of
        Right p -> catMaybes <$> traverse transformBeamPersonToDomain p
        _ -> pure []
    Nothing -> pure []

-- findByMobileNumberAndMerchant ::
--   (Transactionable m) =>
--   Text ->
--   DbHash ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByMobileNumberAndMerchant countryCode mobileNumberHash merchantId = do
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonMobileCountryCode ==. val (Just countryCode)
--         &&. ( person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
--                 ||. person ^. PersonAlternateMobileNumberHash ==. val (Just mobileNumberHash)
--             )
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByMobileNumberAndMerchant :: (L.MonadFlow m, Log m) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchant countryCode mobileNumberHash (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findWithKVConnector
          dbConf'
          Mesh.meshConfig
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

-- findByIdentifierAndMerchant ::
--   Transactionable m =>
--   Id Merchant ->
--   Text ->
--   m (Maybe Person)
-- findByIdentifierAndMerchant merchantId identifier_ =
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonIdentifier ==. val (Just identifier_)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByIdentifierAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByIdentifierAndMerchant (Id merchantId) identifier_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamP.identifier $ Se.Eq $ Just identifier_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

-- findByEmailAndMerchant ::
--   Transactionable m =>
--   Id Merchant ->
--   Text ->
--   m (Maybe Person)
-- findByEmailAndMerchant merchantId email_ =
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonEmail ==. val (Just email_)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByEmailAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByEmailAndMerchant (Id merchantId) email_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.And [Se.Is BeamP.email $ Se.Eq $ Just email_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right (Just p) -> transformBeamPersonToDomain p
        _ -> pure Nothing
    Nothing -> pure Nothing

-- findByRoleAndMobileNumberAndMerchantId ::
--   (Transactionable m, EncFlow m r) =>
--   Role ->
--   Text ->
--   Text ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber_ merchantId = do
--   mobileNumberDbHash <- getDbHash mobileNumber_
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonRole ==. val role_
--         &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
--         &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByRoleAndMobileNumberAndMerchantId :: (L.MonadFlow m, Log m, EncFlow m r) => Role -> Text -> Text -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber_ (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  mobileNumberDbHash <- getDbHash mobileNumber_
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findWithKVConnector
          dbConf'
          Mesh.meshConfig
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

findAllDriverIdExceptProvided :: Transactionable m => Id Merchant -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvided merchantId driverIdsToBeExcluded = do
  res <- Esq.findAll $ do
    (person :& driver) <- from personDriverTable
    where_ $
      person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. not_ ((driver ^. DriverInformationDriverId) `Esq.in_` valList (map (toKey . driverIdToPersonId) driverIdsToBeExcluded))
        &&. driver ^. DriverInformationVerified
        &&. driver ^. DriverInformationEnabled
    return $ driver ^. DriverInformationDriverId
  pure $ personIdToDrivrId <$> res
  where
    personIdToDrivrId :: Id Person -> Id Driver
    personIdToDrivrId = cast

    driverIdToPersonId :: Id Driver -> Id Person
    driverIdToPersonId = cast

findAllDriverIdExceptProvided' :: (L.MonadFlow m, Log m) => Id Merchant -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvided' (Id merchantId) driverIdsToBeExcluded = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      person <- do
        person' <-
          KV.findAllWithKVConnector
            dbConf'
            Mesh.meshConfig
            [Se.Is BeamP.merchantId $ Se.Eq merchantId]
        case person' of
          Left _ -> pure []
          Right x -> catMaybes <$> traverse transformBeamPersonToDomain x

      infoList <-
        either (pure []) (QueriesDI.transformBeamDriverInformationToDomain <$>)
          <$> KV.findAllWithKVConnector
            dbConf'
            Mesh.meshConfig
            [ Se.And
                ( [Se.Is BeamDI.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> person]
                    <> [Se.Is BeamDI.verified $ Se.Eq True]
                    <> [Se.Is BeamDI.enabled $ Se.Eq True]
                    <> [Se.Is BeamDI.driverId $ Se.Not $ Se.In $ getId <$> driverIdsToBeExcluded]
                )
            ]

      pure (map (personIdToDrivrId . DDI.driverId) infoList)
    Nothing -> pure []
  where
    personIdToDrivrId :: Id Person -> Id Driver
    personIdToDrivrId = cast

-- where

-- Nothing -> pure []

-- updateMerchantIdAndMakeAdmin :: Id Person -> Id Merchant -> SqlDB ()
-- updateMerchantIdAndMakeAdmin personId merchantId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonMerchantId =. val (toKey merchantId),
--         PersonRole =. val Person.ADMIN,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateMerchantIdAndMakeAdmin :: (L.MonadFlow m, MonadTime m) => Id Person -> Id Merchant -> m (MeshResult ())
updateMerchantIdAndMakeAdmin (Id personId) (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamP.merchantId merchantId,
          Se.Set BeamP.role Person.ADMIN,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateName :: Id Person -> Text -> SqlDB ()
-- updateName personId name = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonFirstName =. val name,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateName :: (L.MonadFlow m, MonadTime m) => Id Person -> Text -> m (MeshResult ())
updateName (Id personId) name = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamP.firstName name,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updatePersonRec :: Id Person -> Person -> SqlDB ()
-- updatePersonRec personId person = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonFirstName =. val (person.firstName),
--         PersonMiddleName =. val (person.middleName),
--         PersonLastName =. val (person.lastName),
--         PersonRole =. val (person.role),
--         PersonGender =. val (person.gender),
--         PersonEmail =. val (person.email),
--         PersonIdentifier =. val (person.identifier),
--         PersonRating =. val (person.rating),
--         PersonLanguage =. val (person.language),
--         PersonDeviceToken =. val (person.deviceToken),
--         PersonMerchantId =. val (toKey person.merchantId),
--         PersonDescription =. val (person.description),
--         PersonUpdatedAt =. val now,
--         PersonClientVersion =. val (versionToText <$> person.clientVersion),
--         PersonBundleVersion =. val (versionToText <$> person.bundleVersion)
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonRec :: (L.MonadFlow m, MonadTime m) => Id Person -> Person -> m (MeshResult ())
updatePersonRec (Id personId) person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
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

-- updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB ()
-- updatePersonVersions person mbBundleVersion mbClientVersion =
--   when
--     ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
--     do
--       now <- getCurrentTime
--       let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
--           mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
--       Esq.update $ \tbl -> do
--         set
--           tbl
--           [ PersonUpdatedAt =. val now,
--             PersonClientVersion =. val mbClientVersionText,
--             PersonBundleVersion =. val mbBundleVersionText
--           ]
--         where_ $
--           tbl ^. PersonTId ==. val (toKey person.id)

updatePersonVersions :: (L.MonadFlow m, MonadTime m) => Person -> Maybe Version -> Maybe Version -> m ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      dbConf <- L.getOption KBT.PsqlDbCfg
      case dbConf of
        Just dbConf' ->
          void $
            KV.updateWoReturningWithKVConnector
              dbConf'
              Mesh.meshConfig
              [ Se.Set BeamP.clientVersion mbClientVersionText,
                Se.Set BeamP.bundleVersion mbBundleVersionText,
                Se.Set BeamP.updatedAt now
              ]
              [Se.Is BeamP.id (Se.Eq $ getId person.id)]
        Nothing -> pure ()

-- updateDeviceToken :: Id Person -> Maybe FCMRecipientToken -> SqlDB ()
-- updateDeviceToken personId mbDeviceToken = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonDeviceToken =. val mbDeviceToken,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateDeviceToken :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe FCMRecipientToken -> m (MeshResult ())
updateDeviceToken (Id personId) mbDeviceToken = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamP.deviceToken mbDeviceToken,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB ()
-- updateWhatsappNotificationEnrollStatus personId enrollStatus = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateWhatsappNotificationEnrollStatus :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe Whatsapp.OptApiMethods -> m ()
updateWhatsappNotificationEnrollStatus (Id personId) enrollStatus = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.Set BeamP.whatsappNotificationEnrollStatus enrollStatus,
            Se.Set BeamP.updatedAt now
          ]
          [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure ()

-- updateMobileNumberAndCode :: Person -> SqlDB ()
-- updateMobileNumberAndCode person = do
--   let personT = toTType person
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonMobileCountryCode =. val (TPerson.mobileCountryCode personT),
--         PersonMobileNumberEncrypted =. val (TPerson.mobileNumberEncrypted personT),
--         PersonMobileNumberHash =. val (TPerson.mobileNumberHash personT),
--         PersonUnencryptedMobileNumber =. val (TPerson.unencryptedMobileNumber personT),
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey person.id)

updateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m, Log m, EncFlow m r) => Person -> m (MeshResult ())
updateMobileNumberAndCode person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
          Se.Set BeamP.mobileNumberEncrypted $ person.mobileNumber <&> unEncrypted . (.encrypted),
          Se.Set BeamP.mobileNumberHash $ person.mobileNumber <&> (.hash),
          Se.Set BeamP.unencryptedMobileNumber $ person.unencryptedMobileNumber,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq $ getId person.id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- setIsNewFalse :: Id Person -> SqlDB ()
-- setIsNewFalse personId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonIsNew =. val False,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

setIsNewFalse :: (L.MonadFlow m, MonadTime m) => Id Person -> m ()
setIsNewFalse (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.Set BeamP.isNew False,
            Se.Set BeamP.updatedAt now
          ]
          [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure ()

-- deleteById :: Id Person -> SqlDB ()
-- deleteById = Esq.deleteByKey @PersonT

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure ()

-- updateAverageRating :: Id Person -> Centesimal -> SqlDB ()
-- updateAverageRating personId newAverageRating = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonRating =. val (Just newAverageRating),
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateAverageRating :: (L.MonadFlow m, MonadTime m) => Id Person -> Centesimal -> m (MeshResult ())
updateAverageRating (Id personId) newAverageRating = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
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
  (Transactionable m, MonadTime m) =>
  Maybe Variant ->
  LatLong ->
  Int ->
  Id Merchant ->
  Bool ->
  Maybe Seconds ->
  m [NearestDriversResult]
getNearestDrivers mbVariant LatLong {..} radiusMeters merchantId onlyNotOnRide mbDriverPositionInfoExpiry = do
  res <- do
    driverLocs <- getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry LatLong {..} radiusMeters
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

buildFullDriverList :: HashMap.HashMap Text DriverLocation -> HashMap.HashMap Text Vehicle -> HashMap.HashMap Text DriverInformation -> LatLong -> Maybe Variant -> Person -> Maybe (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode)
buildFullDriverList locationHashMap vehicleHashMap driverInfoHashMap LatLong {..} mbVariant person =
  let driverId' = person.id.getId
      location = HashMap.lookupDefault (error "Location not found") driverId' locationHashMap
      vehicle = HashMap.lookupDefault (error "Vehicle not found") driverId' vehicleHashMap
      info = HashMap.lookupDefault (error "Person not found") driverId' driverInfoHashMap
      dist = realToFrac $ distanceBetweenInMeters LatLong {..} LatLong {lat = location.lat, lon = location.lon}
   in if Mb.isNothing mbVariant || Just vehicle.variant == mbVariant
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
  (Transactionable m, MonadTime m) =>
  Id Merchant ->
  Maybe Seconds ->
  LatLong ->
  Int ->
  m [DriverLocation]
getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry LatLong {..} radiusMeters = do
  now <- getCurrentTime
  Esq.findAll $ do
    driverLocs <- from $ table @DriverLocationT
    where_ $
      driverLocs ^. DriverLocationMerchantId ==. (val . toKey $ merchantId)
        &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                ||. (driverLocs ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
            )
        &&. buildRadiusWithin (driverLocs ^. DriverLocationPoint) (lat, lon) (val radiusMeters)
    orderBy [asc (driverLocs ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon))]
    return driverLocs

getDriverInfosWithCond ::
  Transactionable m =>
  [DriverLocation] ->
  Bool ->
  Bool ->
  m [DriverInformation]
getDriverInfosWithCond driverLocs onlyNotOnRide onlyOnRide = do
  Esq.findAll $ do
    driverInfos <- from $ table @DriverInformationT
    where_ $
      driverInfos ^. DriverInformationDriverId `in_` valList personsKeys
        &&. ((Esq.isNothing (driverInfos ^. DriverInformationMode) &&. driverInfos ^. DriverInformationActive) ||. (not_ (Esq.isNothing (driverInfos ^. DriverInformationMode)) &&. (driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.SILENT) ||. driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.ONLINE))))
        &&. (if onlyNotOnRide then not_ (driverInfos ^. DriverInformationOnRide) else if onlyOnRide then driverInfos ^. DriverInformationOnRide else val True)
        &&. not_ (driverInfos ^. DriverInformationBlocked)
    return driverInfos
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromLocations driverLocs

getVehiclesWithCond ::
  Transactionable m =>
  [DriverInformation] ->
  m [Vehicle]
getVehiclesWithCond driverInfo = do
  Esq.findAll $ do
    vehicles <- from $ table @VehicleT
    where_ $
      vehicles ^. VehicleDriverId `in_` valList personsKeys
    return vehicles
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromInfo driverInfo

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
  (Transactionable m, MonadTime m) =>
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
    driverLocs <- getDriverLocsFromMerchId mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId
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

-- this query is incomplete and not being used currently -- don't forget to add argument LatLong {..} and datatype while completing the query -- TODO
getNearestDriversCurrentlyOnRide' :: (L.MonadFlow m, Log m, MonadTime m) => Maybe Variant -> Int -> Id Merchant -> Maybe Seconds -> Int -> m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide' mbVariant radiusMeters (Id merchantId') mbDriverPositionInfoExpiry reduceRadiusValue = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  -- let distanceFromDestinationToPickup = Utils.getPoint (lat, lon)
  let onRideRadius = (fromIntegral (radiusMeters - reduceRadiusValue) :: Double)
  case dbConf of
    Just dbCOnf' -> do
      personList <- do
        p <-
          KV.findAllWithOptionsKVConnector
            dbCOnf'
            Mesh.meshConfig
            [ Se.And
                [ Se.Is BeamP.role $ Se.Eq Person.DRIVER,
                  Se.Is BeamP.merchantId $ Se.Eq merchantId'
                ]
            ]
            (Se.Asc BeamP.firstName)
            Nothing
            Nothing
        case p of
          Left _ -> pure []
          Right x -> catMaybes <$> traverse transformBeamPersonToDomain x

      dlList <- findAllDriverLocations' dbCOnf' (getId . (Person.id :: PersonE e -> Id Person) <$> personList) mbDriverPositionInfoExpiry now

      driverInfoList <-
        either (pure []) (QueriesDI.transformBeamDriverInformationToDomain <$>)
          <$> KV.findAllWithKVConnector
            dbCOnf'
            Mesh.meshConfig
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

      vehicleList <-
        either (pure []) (QV.transformBeamVehicleToDomain <$>)
          <$> KV.findAllWithKVConnector
            dbCOnf'
            Mesh.meshConfig
            [Se.Is BeamV.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]

      driverQuoteList <- do
        diverQL <-
          KV.findAllWithKVConnector
            dbCOnf'
            Mesh.meshConfig
            [Se.Is BeamDQ.driverId $ Se.In $ getId . (Person.id :: PersonE e -> Id Person) <$> personList]
        case diverQL of
          Right x -> catMaybes <$> traverse QDQ.transformBeamDriverQuoteToDomain x
          _ -> pure []

      bookingList <- do
        bookingL <-
          KV.findAllWithKVConnector
            dbCOnf'
            Mesh.meshConfig
            [ Se.And
                [ Se.Is BeamB.quoteId $ Se.In $ getId . DriverQuote.id <$> driverQuoteList,
                  Se.Is BeamB.status $ Se.Eq Booking.TRIP_ASSIGNED
                ] -- Se.Is Se.LessThan $ Utils.getPoint( BeamB.toLocation.lat, BeamB.toLocation.lon) <->. DDL. <$> dlList
            ]
        case bookingL of
          Right x -> catMaybes <$> traverse QB.transformBeamBookingToDomain x
          _ -> pure []

      let pDl = foldl' (getPersonWithlocation dlList) [] personList
      let pDlInfo = foldl' (getPersonWithInfo driverInfoList) [] pDl
      let pDlInfoVeh = foldl' (getPersonDInfoDLVehicle mbVariant vehicleList) [] pDlInfo
      let pDlInfoVehQuote = foldl' (getPersonDInfoDLVehQuote driverQuoteList) [] pDlInfoVeh
      let pDlInfoVehQuoteBooking = foldl' (getPersonDInfoDLVehQuoteBooking bookingList) [] pDlInfoVehQuote
      let pDlInfoVehQuoteBooking' = map (\(person', dl', info', vehicle', _, booking') -> (person'.id, person'.deviceToken, person'.language, info'.onRide, info'.canDowngradeToSedan, info'.canDowngradeToHatchback, info'.canDowngradeToTaxi, dl'.lat, dl'.lon, vehicle'.variant, booking'.toLocation.lat, booking'.toLocation.lat, onRideRadius, onRideRadius, info'.mode)) pDlInfoVehQuoteBooking
      pure $ makeNearestDriversResult =<< pDlInfoVehQuoteBooking'
    Nothing -> pure []
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

    findAllDriverLocations' dbCOnf' driverIds mbDriverPositionInfoExpiry' now = do
      conn <- L.getOrInitSqlConn dbCOnf'
      case conn of
        Right c -> do
          geoms <-
            L.runDB c $
              L.findRows $
                B.select $
                  B.filter_'
                    ( \BeamDL.DriverLocationT {..} ->
                        if Kernel.Prelude.isNothing mbDriverPositionInfoExpiry'
                          then B.sqlBool_ $ B.val_ True
                          else B.sqlBool_ (coordinatesCalculatedAt B.>=. B.val_ now) B.&&?. B.sqlBool_ (driverId `B.in_` (B.val_ <$> driverIds))
                    )
                    $ B.all_ (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
          pure (either (const []) (QDL.transformBeamDriverLocationToDomain <$>) geoms)
        Left _ -> pure []

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

-- this query is incomplete and not being used currently -- don't forget to add argument LatLong {..} and datatype while completing the query -- TODO

-- updateAlternateMobileNumberAndCode :: Person -> SqlDB ()
-- updateAlternateMobileNumberAndCode person = do
--   now <- getCurrentTime
--   let personT = toTType person
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonAlternateMobileNumberEncrypted =. val (TPerson.alternateMobileNumberEncrypted personT),
--         PersonUnencryptedAlternateMobileNumber =. val (TPerson.unencryptedAlternateMobileNumber personT),
--         PersonAlternateMobileNumberHash =. val (TPerson.alternateMobileNumberHash personT),
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey person.id)

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
buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap latLon onRideRadius mbVariant vehicle =
  let driverId' = vehicle.driverId.getId
      location = HashMap.lookupDefault (error "Location not found") driverId' locationHashMap
      quote = HashMap.lookupDefault (error "Quote not found") driverId' quotesHashMap
      booking = HashMap.lookupDefault (error "Booking not found") quote.id.getId bookingHashMap
      bookingLocation = HashMap.lookupDefault (error "Booking Location not found") booking.toLocation.id.getId bookingLocsHashMap
      info = HashMap.lookupDefault (error "Info not found") driverId' driverInfoHashMap
      person = HashMap.lookupDefault (error "Person not found") driverId' personHashMap
      driverLocationPoint = LatLong {lat = location.lat, lon = location.lon}
      destinationPoint = LatLong {lat = bookingLocation.lat, lon = bookingLocation.lon}
      distanceFromDriverToDestination = realToFrac $ distanceBetweenInMeters driverLocationPoint destinationPoint
      distanceFromDestinationToPickup = realToFrac $ distanceBetweenInMeters latLon destinationPoint
      onRideRadiusValidity = (distanceFromDriverToDestination + distanceFromDestinationToPickup) < onRideRadius
   in if onRideRadiusValidity
        && Mb.isNothing mbVariant || Just vehicle.variant == mbVariant
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
        then Just (person.id, person.deviceToken, person.language, info.onRide, info.canDowngradeToSedan, info.canDowngradeToHatchback, info.canDowngradeToTaxi, location.lat, location.lon, vehicle.variant, bookingLocation.lat, bookingLocation.lon, distanceFromDriverToDestination + distanceFromDestinationToPickup, distanceFromDriverToDestination, info.mode)
        else Nothing

updateAlternateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m) => Person -> m (MeshResult ())
updateAlternateMobileNumberAndCode person = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
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
  if isJust bundleVersion && isJust clientVersion
    then
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
    else pure Nothing

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
