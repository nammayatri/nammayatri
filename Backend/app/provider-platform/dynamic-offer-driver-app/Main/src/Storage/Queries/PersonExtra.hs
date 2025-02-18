module Storage.Queries.PersonExtra
  ( module Storage.Queries.PersonExtra,
    module Reexport,
  )
where

-- Extra code goes here --

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HashMap
import qualified Database.Beam as B
import Database.Beam.Postgres hiding ((++.))
import qualified Database.Beam.Query ()
import qualified Domain.Types.Booking as Booking
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import qualified Domain.Types.DriverLocation as DDL
import Domain.Types.DriverQuote as DriverQuote
import Domain.Types.Merchant hiding (MerchantAPIEntity)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Domain.Types.Vehicle
import qualified EulerHS.Language as L
import IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.Version
import qualified Sequelize as Se
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.DriverLicense as BeamDL
import qualified Storage.Beam.DriverQuote as BeamDQ
import qualified Storage.Beam.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Ride as BeamR
import qualified Storage.Beam.Vehicle as BeamV
import qualified Storage.Beam.VehicleRegistrationCertificate as BeamVRC
import Storage.Queries.Booking ()
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLicense ()
import Storage.Queries.DriverQuote ()
import qualified Storage.Queries.DriverRCAssociation ()
import Storage.Queries.OrphanInstances.DriverInformation ()
import Storage.Queries.OrphanInstances.Person ()
import Storage.Queries.Person.GetNearestDrivers as Reexport
import Storage.Queries.Person.GetNearestDriversCurrentlyOnRide as Reexport
import Storage.Queries.Person.GetNearestGoHomeDrivers as Reexport
import qualified Storage.Queries.Person.Internal as Int
import Storage.Queries.Ride ()
import Storage.Queries.Vehicle ()
import qualified Storage.Queries.Vehicle.Internal as Int
import qualified Storage.Queries.VehicleRegistrationCertificate ()

getDriversByIdIn :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Person] -> m [Person]
getDriversByIdIn personIds = findAllWithKV [Se.Is BeamP.id $ Se.In $ getId <$> personIds]

updateMerchantIdAndMakeAdmin :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id Merchant -> m ()
updateMerchantIdAndMakeAdmin (Id personId) (Id merchantId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.merchantId merchantId,
      Se.Set BeamP.role ADMIN,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

findAdminsByMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m [Person]
findAdminsByMerchantId (Id merchantId) = findAllWithDb [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.Eq Person.ADMIN]]

findAllByPersonIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> m [Person]
findAllByPersonIds ids = findAllWithDb [Se.Is BeamP.id $ Se.In ids]

findPersonIdsByPhoneNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => [Text] -> m [Person]
findPersonIdsByPhoneNumber phoneNumbers = do
  phoneNumbersHashes <- mapM getDbHash phoneNumbers
  let mbhashes = Just <$> phoneNumbersHashes
  findAllWithDb [Se.Is BeamP.mobileNumberHash $ Se.In mbhashes]

findByEmail :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> m (Maybe Person)
findByEmail email = findOneWithKV [Se.Is BeamP.email $ Se.Eq email]

findByEmailAndMerchantIdAndRole :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Id Merchant -> Role -> m (Maybe Person)
findByEmailAndMerchantIdAndRole email (Id merchantId) role_ = findOneWithKV [Se.And [Se.Is BeamP.email $ Se.Eq email, Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.Eq role_]]

findAllDriversWithInfoAndVehicle ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Merchant ->
  DMOC.MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  Maybe Text ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchant opCity limitVal offsetVal mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString mbNameSearchString = do
  dbConf <- getReplicaBeamConfig
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limitVal) $
          B.offset_ (fromIntegral offsetVal) $
            B.filter_'
              ( \(person, driverInfo, vehicle) ->
                  person.merchantId B.==?. B.val_ (getId merchant.id)
                    B.&&?. (person.merchantOperatingCityId B.==?. B.val_ (Just $ getId opCity.id) B.||?. (B.sqlBool_ (B.isNothing_ person.merchantOperatingCityId) B.&&?. B.sqlBool_ (B.val_ (merchant.city == opCity.city))))
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\vehNum -> B.maybe_ (B.sqlBool_ $ B.val_ False) (\rNo -> B.sqlBool_ (B.like_ rNo (B.val_ ("%" <> vehNum <> "%")))) vehicle.registrationNo) mbVehicleNumberSearchString
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.like_ person.firstName (B.val_ ("%" <> name <> "%")))) mbNameSearchString
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.maybe_ (B.sqlBool_ $ B.val_ False) (\middleName -> B.sqlBool_ (B.like_ middleName (B.val_ ("%" <> name <> "%")))) person.middleName) mbNameSearchString
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.maybe_ (B.sqlBool_ $ B.val_ False) (\lastName -> B.sqlBool_ (B.like_ lastName (B.val_ ("%" <> name <> "%")))) person.lastName) mbNameSearchString
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
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [DriverInformation] ->
  m [Person]
getDriversList driverInfos = findAllWithKV [Se.Is BeamP.id $ Se.In personsKeys]
  where
    personsKeys = getId <$> fetchDriverIDsFromInfo driverInfos

getDriverInformations ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) =>
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

data FullDriver = FullDriver
  { person :: Person,
    location :: DriverLocation,
    info :: DriverInformation,
    vehicle :: Vehicle
  }
  deriving (Generic)

findAllDriversByIdsFirstNameAsc ::
  (Functor m, MonadFlow m, LT.HasLocationService m r, CoreMetrics m, CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc _merchantId driverIds = do
  driverLocs <- LF.driversLocation driverIds
  driverInfos <- Int.getDriverInfos $ map ((.getId) . DDL.driverId) driverLocs
  vehicle <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicle
  return (linkArrays driverLocs driverInfos vehicle drivers)
  where
    linkArrays driverLocations driverInformations vehicles persons =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
          driverInfoHashMap = HashMap.fromList $ (\di -> (di.driverId, di)) <$> driverInformations
       in mapMaybe (buildFullDriver personHashMap vehicleHashMap driverInfoHashMap) driverLocations

    buildFullDriver personHashMap vehicleHashMap driverInfoHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      Just $ FullDriver person location info vehicle

data DriverInfosAndRideDetails = DriverInfosAndRideDetails
  { driverInfo :: DriverInformation,
    ride :: Ride.Ride
  }

getOnRideStuckDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => UTCTime -> m [DriverInfosAndRideDetails]
getOnRideStuckDriverIds dbSyncInterVal = do
  rideDetails <- findAllWithDb [Se.Is BeamR.status $ Se.In [Ride.INPROGRESS, Ride.NEW]]
  let driverIds = Ride.driverId <$> rideDetails
  driverInfos <- findAllWithDb [Se.And [Se.Is BeamDI.onRide $ Se.Eq True, Se.Is BeamDI.updatedAt $ Se.LessThanOrEq dbSyncInterVal, Se.Is BeamDI.driverId $ Se.Not $ Se.In (getId <$> driverIds)]]
  return (linkArrays driverInfos rideDetails driverIds)
  where
    linkArrays driverInfos rideDetails driverIds =
      let driverInfosHashMap = HashMap.fromList $ (\p -> (p.driverId, p)) <$> driverInfos
          rideDetailsHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> rideDetails
       in mapMaybe (buildDriverInfosAndRideDetails driverInfosHashMap rideDetailsHashMap) driverIds

    buildDriverInfosAndRideDetails driverInfosHashMap rideDetailsHashMap driverId = do
      driverInfo <- HashMap.lookup driverId driverInfosHashMap
      rideDetail <- HashMap.lookup driverId rideDetailsHashMap
      Just $ DriverInfosAndRideDetails driverInfo rideDetail

fetchDriverIDsFromDriverQuotes :: [DriverQuote] -> [Id Person]
fetchDriverIDsFromDriverQuotes = map DriverQuote.driverId

fetchQuoteIdFromDriverQuotes :: [DriverQuote] -> [Text]
fetchQuoteIdFromDriverQuotes = map (.id.getId)

fetchDriverIDsFromLocations :: [DriverLocation] -> [Id Person]
fetchDriverIDsFromLocations = map DriverLocation.driverId

fetchDriverIDsFromInfo :: [DriverInformation] -> [Id Person]
fetchDriverIDsFromInfo = map DriverInfo.driverId

findAllDriverInformationWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => [Se.Clause Postgres BeamDI.DriverInformationT] -> m [DriverInformation]
findAllDriverInformationWithSeConditions = findAllWithKV

findAllVehiclesWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamV.VehicleT] -> m [Vehicle]
findAllVehiclesWithSeConditions = findAllWithKV

findAllBookingsWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamB.BookingT] -> m [Booking.Booking]
findAllBookingsWithSeConditions = findAllWithKV

findAllDriverQuoteWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamDQ.DriverQuoteT] -> m [DriverQuote]
findAllDriverQuoteWithSeConditions = findAllWithKV

findAllPersonWithSeConditionsNameAsc :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditionsNameAsc conditions = findAllWithOptionsKV conditions (Se.Asc BeamP.firstName) Nothing Nothing

findAllPersonWithSeConditions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamP.PersonT] -> m [Person]
findAllPersonWithSeConditions = findAllWithKV

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int
  }

fetchDriverInfo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Merchant -> DMOC.MerchantOperatingCity -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> Maybe Text -> Maybe (Id Person) -> m (Maybe (Person, DriverInformation, Maybe Vehicle))
fetchDriverInfo merchant moCity mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash mbEmail mbPersonId = do
  dbConf <- getReplicaBeamConfig
  now <- getCurrentTime
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(person, _driverInfo, vehicle, driverLicense, _driverRCAssociation, vehicleRegistrationCertificate) ->
              person.merchantId B.==?. B.val_ merchant.id.getId
                B.&&?. (person.merchantOperatingCityId B.==?. B.val_ (Just $ getId moCity.id) B.||?. (B.sqlBool_ (B.isNothing_ person.merchantOperatingCityId) B.&&?. B.sqlBool_ (B.val_ (merchant.city == moCity.city))))
                B.&&?. person.role B.==?. B.val_ Person.DRIVER
                B.&&?. maybe
                  (B.sqlBool_ $ B.val_ True)
                  ( \(mobileNumberDbHash, mobileCountryCode) ->
                      person.mobileCountryCode B.==?. B.val_ (Just mobileCountryCode)
                        B.&&?. (person.mobileNumberHash B.==?. B.val_ (Just mobileNumberDbHash) B.||?. person.alternateMobileNumberHash B.==?. B.val_ (Just mobileNumberDbHash))
                  )
                  mbMobileNumberDbHashWithCode
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\vehicleNo -> vehicle.registrationNo B.==?. B.val_ (Just vehicleNo)) mbVehicleNumber
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\dlNumberHash -> driverLicense.licenseNumberHash B.==?. B.val_ (Just dlNumberHash)) mbDlNumberHash
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rcNumberHash -> vehicleRegistrationCertificate.certificateNumberHash B.==?. B.val_ (Just rcNumberHash)) mbRcNumberHash
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\email -> person.email B.==?. B.val_ (Just email)) mbEmail
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\personId -> person.id B.==?. B.val_ (getId personId)) mbPersonId
          )
          do
            person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
            driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\info' -> BeamP.id person B.==?. BeamDI.driverId info')
            vehicle <- B.leftJoin_' (B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)) (\veh' -> BeamP.id person B.==?. BeamV.driverId veh')
            driverLicense <- B.leftJoin_' (B.all_ (BeamCommon.driverLicense BeamCommon.atlasDB)) (\dl' -> maybe (B.sqlBool_ $ B.val_ False) (\_ -> BeamP.id person B.==?. BeamDL.driverId dl') mbDlNumberHash)
            driverRCAssociation <- B.leftJoin_' (B.all_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB)) (\drca' -> maybe (B.sqlBool_ $ B.val_ False) (\_ -> BeamP.id person B.==?. BeamDRCA.driverId drca' B.&&?. B.sqlBool_ (B.just_ (B.val_ now) B.<. BeamDRCA.associatedTill drca')) mbRcNumberHash)
            vehicleRegistrationCertificate <- B.leftJoin_' (B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)) (\vrc' -> maybe (B.sqlBool_ $ B.val_ False) (\_ -> BeamDRCA.rcId driverRCAssociation B.==?. B.just_ (BeamVRC.id vrc')) mbRcNumberHash)
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

findByRoleAndMobileNumberAndMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Role -> Text -> Text -> Id Merchant -> m (Maybe Person)
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

findByMobileNumberAndMerchantAndRole :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DbHash -> Id Merchant -> Role -> m (Maybe Person)
findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash (Id merchantId) mbRole =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Is BeamP.merchantId $ Se.Eq merchantId,
          Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash],
          Se.Is BeamP.role $ Se.Eq mbRole
        ]
    ]

updatePersonRec :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Person -> m ()
updatePersonRec (Id personId) person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.firstName $ person.firstName,
      Se.Set BeamP.middleName $ person.middleName,
      Se.Set BeamP.lastName $ person.lastName,
      Se.Set BeamP.role $ person.role,
      Se.Set BeamP.gender $ person.gender,
      Se.Set BeamP.email $ person.email,
      Se.Set BeamP.hometown $ person.hometown,
      Se.Set BeamP.languagesSpoken $ person.languagesSpoken,
      Se.Set BeamP.identifier $ person.identifier,
      Se.Set BeamP.language $ person.language,
      Se.Set BeamP.deviceToken $ person.deviceToken,
      Se.Set BeamP.merchantId $ getId person.merchantId,
      Se.Set BeamP.description $ person.description,
      Se.Set BeamP.updatedAt now,
      Se.Set BeamP.clientSdkVersion (versionToText <$> person.clientSdkVersion),
      Se.Set BeamP.clientBundleVersion (versionToText <$> person.clientBundleVersion),
      Se.Set BeamP.clientConfigVersion (versionToText <$> person.clientConfigVersion),
      Se.Set BeamP.clientOsVersion (deviceVersion <$> person.clientDevice),
      Se.Set BeamP.clientOsType (deviceType <$> person.clientDevice),
      Se.Set BeamP.clientModelName (deviceModel <$> person.clientDevice),
      Se.Set BeamP.clientManufacturer (deviceManufacturer =<< person.clientDevice),
      Se.Set BeamP.backendConfigVersion (versionToText <$> person.backendConfigVersion),
      Se.Set BeamP.backendAppVersion (person.backendAppVersion)
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatePersonVersionsAndMerchantOperatingCity ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Id DMOC.MerchantOperatingCity ->
  m ()
updatePersonVersionsAndMerchantOperatingCity person mbBundleVersion mbClientVersion mbConfigVersion mbClientId mbDevice' mbBackendApp city = do
  let mbDevice = getDeviceFromText mbDevice'
  let isBundleDataPresent = isJust mbBundleVersion || isJust mbClientVersion || isJust mbDevice' || isJust mbConfigVersion
  let isAnyMismatchPresent = or [person.clientBundleVersion /= mbBundleVersion, person.clientSdkVersion /= mbClientVersion, person.clientConfigVersion /= mbConfigVersion, person.clientDevice /= mbDevice, person.backendAppVersion /= mbBackendApp, person.merchantOperatingCityId /= city]
  when (isBundleDataPresent && isAnyMismatchPresent) $ do
    now <- getCurrentTime
    let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.clientBundleVersion)
        mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientSdkVersion)
        mbConfigVersionText = versionToText <$> (mbConfigVersion <|> person.clientConfigVersion)
        mbOsVersion = deviceVersion <$> (mbDevice <|> person.clientDevice)
        mbOsType = deviceType <$> (mbDevice <|> person.clientDevice)
        mbModelName = deviceModel <$> (mbDevice <|> person.clientDevice)
        mbClientId' = mbClientId <|> person.clientId
        mbManufacturer = deviceManufacturer =<< (mbDevice <|> person.clientDevice)
    updateOneWithKV
      [ Se.Set BeamP.clientSdkVersion mbClientVersionText,
        Se.Set BeamP.clientBundleVersion mbBundleVersionText,
        Se.Set BeamP.clientConfigVersion mbConfigVersionText,
        Se.Set BeamP.clientOsVersion mbOsVersion,
        Se.Set BeamP.clientOsType mbOsType,
        Se.Set BeamP.clientId mbClientId',
        Se.Set BeamP.clientModelName mbModelName,
        Se.Set BeamP.clientManufacturer mbManufacturer,
        Se.Set BeamP.backendAppVersion mbBackendApp,
        Se.Set BeamP.updatedAt now,
        Se.Set BeamP.merchantOperatingCityId $ Just city.getId
      ]
      [Se.Is BeamP.id (Se.Eq $ getId person.id)]

updateAlternateMobileNumberAndCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Person -> m ()
updateAlternateMobileNumberAndCode person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.alternateMobileNumberEncrypted (person.alternateMobileNumber <&> unEncrypted . (.encrypted)),
      Se.Set BeamP.alternateMobileNumberHash (person.alternateMobileNumber <&> (.hash)),
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq $ getId person.id)]

findAllPersonWithDriverInfos :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [DriverInformation] -> Id Merchant -> m [Person]
findAllPersonWithDriverInfos dInfos merchantId = findAllWithKV [Se.And [Se.Is BeamP.id $ Se.In (getId . DriverInfo.driverId <$> dInfos), Se.Is BeamP.merchantId $ Se.Eq (getId merchantId)]]

findAllPersonAndDriverInfoWithDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Person] -> m [(Person, DriverInformation)]
findAllPersonAndDriverInfoWithDriverIds driverIds = do
  let allDriverIds = map (\driverId -> driverId.getId) driverIds
  dbConf <- getReplicaBeamConfig
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(person, _) ->
              B.sqlBool_ $ person.id `B.in_` (B.val_ <$> allDriverIds)
          )
          do
            person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
            driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\info' -> BeamP.id person B.==?. BeamDI.driverId info')
            pure (person, driverInfo)
  case result of
    Right x -> do
      let persons = fmap fst x
          driverInfos = fmap snd x
      p <- catMaybes <$> mapM fromTType' persons
      di <- catMaybes <$> mapM fromTType' driverInfos
      pure $ zip p di
    Left _ -> pure []

updateMediaId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Maybe (Id MediaFile) -> m ()
updateMediaId (Id driverId) faceImageId = updateWithKV [Se.Set BeamP.faceImageId (getId <$> faceImageId)] [Se.Is BeamP.id $ Se.Eq driverId]

findAllMerchantIdByPhoneNo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DbHash -> m [Person]
findAllMerchantIdByPhoneNo countryCode mobileNumberHash =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
          Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash]
        ]
    ]

updateMerchantOperatingCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id DMOC.MerchantOperatingCity -> m ()
updateMerchantOperatingCityId (Id driverId) (Id opCityId) = updateWithKV [Se.Set BeamP.merchantOperatingCityId (Just opCityId)] [Se.Is BeamP.id $ Se.Eq driverId]

updateMobileNumberAndCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Person -> m ()
updateMobileNumberAndCode person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
      Se.Set BeamP.mobileNumberEncrypted $ person.mobileNumber <&> unEncrypted . (.encrypted),
      Se.Set BeamP.mobileNumberHash $ person.mobileNumber <&> (.hash),
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq $ getId person.id)]

updatePersonDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Person -> m ()
updatePersonDetails person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.firstName $ person.firstName,
      Se.Set BeamP.lastName $ person.lastName,
      Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
      Se.Set BeamP.mobileNumberEncrypted $ person.mobileNumber <&> unEncrypted . (.encrypted),
      Se.Set BeamP.email $ person.email,
      Se.Set BeamP.mobileNumberHash $ person.mobileNumber <&> (.hash),
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq $ getId person.id)]

updateFleetOwnerDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Person -> Person -> m ()
updateFleetOwnerDetails (Id personId) req = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamP.updatedAt now]
        <> [Se.Set BeamP.firstName req.firstName]
        <> [Se.Set BeamP.lastName req.lastName | isJust req.lastName]
        <> [Se.Set BeamP.mobileCountryCode req.mobileCountryCode | isJust req.mobileCountryCode]
        <> [Se.Set BeamP.mobileNumberEncrypted $ req.mobileNumber <&> unEncrypted . (.encrypted) | isJust req.mobileNumber]
        <> [Se.Set BeamP.mobileNumberHash (req.mobileNumber <&> (.hash)) | isJust req.mobileNumber]
        <> [Se.Set BeamP.email req.email | isJust req.email]
    )
    [Se.Is BeamP.id (Se.Eq personId)]

clearDeviceTokenByPersonId :: (MonadFlow m, EsqDBFlow m r) => Id Person -> m ()
clearDeviceTokenByPersonId personId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.deviceToken Nothing,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonMobileByFleetRole :: (MonadFlow m, EsqDBFlow m r) => Text -> EncryptedHashed Text -> m ()
updatePersonMobileByFleetRole personId encMobileNumber = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.mobileNumberEncrypted $ Just $ unEncrypted encMobileNumber.encrypted,
      Se.Set BeamP.mobileNumberHash $ Just encMobileNumber.hash,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq personId,
      Se.Is BeamP.role $ Se.Eq Person.FLEET_OWNER
    ]
