{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Mb
import qualified Domain.Types.Booking as Booking
import Domain.Types.Booking.BookingLocation
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.DriverQuote as DriverQuote
import Domain.Types.MediaFile
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Ride as Ride
import Domain.Types.Vehicle as DV
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
import Storage.Queries.DriverQuote (baseDriverQuoteQuery)
import Storage.Queries.FullEntityBuilders (buildFullBooking, buildFullDriverQuote)
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

create :: Person -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Person)
findById = Esq.findById

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
  Maybe Bool ->
  Maybe DbHash ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId limitVal offsetVal mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString = do
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
        &&. maybe (val True) (\subscribed -> info ^. DriverInformationSubscribed ==. val subscribed) mbSubscribed
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

findAllDriversByIdsFirstNameAsc ::
  (Transactionable m, Functor m, EsqLocRepDBFlow m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc merchantId driverIds = do
  driverLocs <- getDriverLocs driverIds merchantId
  driverInfos <- getDriverInfos driverLocs
  vehicle <- getVehicles driverInfos
  drivers <- getDrivers vehicle
  logDebug $ "FindAllDriversByIdsFirstNameAsc - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vec:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
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
  (Transactionable m, EsqLocRepDBFlow m r, MonadFlow m) =>
  [Id Person] ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocs driverIds merchantId = do
  runInLocReplica $
    Esq.findAll $ do
      driverLocs <- from $ table @DriverLocationT
      where_ $
        driverLocs ^. DriverLocationDriverId `in_` valList personsKeys
          &&. driverLocs ^. DriverLocationMerchantId ==. (val . toKey $ merchantId)
      return driverLocs
  where
    personsKeys = toKey . cast <$> driverIds

getDriverInfos ::
  (Transactionable m, EsqDBReplicaFlow m r) =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos driverLocs = do
  runInReplica $
    Esq.findAll $ do
      driverInfos <- from $ table @DriverInformationT
      where_ $
        driverInfos ^. DriverInformationDriverId `in_` valList personsKeys
      return driverInfos
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromLocations driverLocs

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
  (Transactionable m, EsqDBReplicaFlow m r) =>
  [DriverInformation] ->
  m [Vehicle]
getVehicles driverInfo = do
  runInReplica $
    Esq.findAll $ do
      vehicles <- from $ table @VehicleT
      where_ $
        vehicles ^. VehicleDriverId `in_` valList personsKeys
      return vehicles
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromInfo driverInfo

getDrivers ::
  (Transactionable m, EsqDBReplicaFlow m r) =>
  [Vehicle] ->
  m [Person]
getDrivers vehicles = do
  runInReplica $
    Esq.findAll $ do
      persons <- from $ table @PersonT
      where_ $
        persons ^. PersonTId `in_` valList personsKeys
          &&. persons ^. PersonRole ==. val Person.DRIVER
      return persons
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromVehicle vehicles

getDriversByIdIn ::
  Transactionable m =>
  [Id Person] ->
  m [Person]
getDriversByIdIn driverIds = do
  Esq.findAll $ do
    persons <- from $ table @PersonT
    where_ $
      persons ^. PersonTId `in_` valList personIds
    return persons
  where
    personIds = toKey . cast <$> driverIds

getDriversWithMerchID ::
  (Transactionable m, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  m [Person]
getDriversWithMerchID merchantId = do
  runInReplica $
    Esq.findAll $ do
      persons <- from $ table @PersonT
      where_ $
        persons ^. PersonMerchantId ==. val (toKey merchantId)
          &&. persons ^. PersonRole ==. val Person.DRIVER
      return persons

getDriverQuote ::
  Transactionable m =>
  [Person] ->
  m [DriverQuote]
getDriverQuote persons =
  buildDType $ do
    res <- Esq.findAll' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $
        dQuote ^. DriverQuoteDriverId `in_` valList personsKeys
          &&. dQuote ^. DriverQuoteStatus ==. val DriverQuote.Active
      pure (dQuote, farePars)
    catMaybes <$> mapM buildFullDriverQuote res
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromPersons persons

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

getBookingLocs ::
  (Transactionable m, EsqDBReplicaFlow m r) =>
  [Booking.Booking] ->
  m [BookingLocation]
getBookingLocs bookings = do
  runInReplica $
    Esq.findAll $ do
      bookingLoc <- from $ table @BookingLocationT
      where_ $
        bookingLoc ^. BookingLocationTId `in_` valList toLocKeys
      return bookingLoc
  where
    toLocKeys = toKey . cast <$> fetchToLocationIDFromBooking bookings

getDriverLocsFromMerchId ::
  (Transactionable m, MonadTime m, EsqLocRepDBFlow m r) =>
  Maybe Seconds ->
  LatLong ->
  Int ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocsFromMerchId mbDriverPositionInfoExpiry LatLong {..} radiusMeters merchantId = do
  now <- getCurrentTime
  runInLocReplica $
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
fetchDriverInfo merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash =
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
                           &&. just (val True) ==. mbRcAssoc ?. DriverRCAssociationIsRcActive
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
    -- used only for dl and rc entites, because they are not required for final result, only for filters
    joinOnlyWhenJust mbFilter cond = maybe (val False) (const cond) mbFilter

findByIdAndRoleAndMerchantId ::
  Transactionable m =>
  Id Person ->
  Person.Role ->
  Id Merchant ->
  m (Maybe Person)
findByIdAndRoleAndMerchantId pid role_ merchantId =
  Esq.findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonTId ==. val (toKey pid)
        &&. person ^. PersonRole ==. val role_
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findAllByMerchantId ::
  Transactionable m =>
  [Person.Role] ->
  Id Merchant ->
  m [Person]
findAllByMerchantId roles merchantId =
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      (person ^. PersonRole `in_` valList roles ||. val (null roles))
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findAdminsByMerchantId :: Transactionable m => Id Merchant -> m [Person]
findAdminsByMerchantId merchantId =
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. person ^. PersonRole ==. val Person.ADMIN
    return person

findByMobileNumberAndMerchant ::
  (Transactionable m) =>
  Text ->
  DbHash ->
  Id Merchant ->
  m (Maybe Person)
findByMobileNumberAndMerchant countryCode mobileNumberHash merchantId = do
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. ( person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
                ||. person ^. PersonAlternateMobileNumberHash ==. val (Just mobileNumberHash)
            )
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByIdentifierAndMerchant ::
  Transactionable m =>
  Id Merchant ->
  Text ->
  m (Maybe Person)
findByIdentifierAndMerchant merchantId identifier_ =
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonIdentifier ==. val (Just identifier_)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByEmailAndMerchant ::
  Transactionable m =>
  Id Merchant ->
  Text ->
  m (Maybe Person)
findByEmailAndMerchant merchantId email_ =
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmail ==. val (Just email_)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByRoleAndMobileNumberAndMerchantId ::
  (Transactionable m, EncFlow m r) =>
  Role ->
  Text ->
  Text ->
  Id Merchant ->
  m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber_ merchantId = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

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

updateMerchantIdAndMakeAdmin :: Id Person -> Id Merchant -> SqlDB ()
updateMerchantIdAndMakeAdmin personId merchantId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonMerchantId =. val (toKey merchantId),
        PersonRole =. val Person.ADMIN,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateName :: Id Person -> Text -> SqlDB ()
updateName personId name = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFirstName =. val name,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonRec :: Id Person -> Person -> SqlDB ()
updatePersonRec personId person = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFirstName =. val (person.firstName),
        PersonMiddleName =. val (person.middleName),
        PersonLastName =. val (person.lastName),
        PersonRole =. val (person.role),
        PersonGender =. val (person.gender),
        PersonEmail =. val (person.email),
        PersonHometown =. val (person.hometown),
        PersonLanguagesSpoken =. val (PostgresList <$> person.languagesSpoken),
        PersonIdentifier =. val (person.identifier),
        PersonRating =. val (person.rating),
        PersonLanguage =. val (person.language),
        PersonDeviceToken =. val (person.deviceToken),
        PersonMerchantId =. val (toKey person.merchantId),
        PersonDescription =. val (person.description),
        PersonUpdatedAt =. val now,
        PersonClientVersion =. val (versionToText <$> person.clientVersion),
        PersonBundleVersion =. val (versionToText <$> person.bundleVersion)
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      Esq.update $ \tbl -> do
        set
          tbl
          [ PersonUpdatedAt =. val now,
            PersonClientVersion =. val mbClientVersionText,
            PersonBundleVersion =. val mbBundleVersionText
          ]
        where_ $
          tbl ^. PersonTId ==. val (toKey person.id)

updateDeviceToken :: Id Person -> Maybe FCMRecipientToken -> SqlDB ()
updateDeviceToken personId mbDeviceToken = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonDeviceToken =. val mbDeviceToken,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB ()
updateWhatsappNotificationEnrollStatus personId enrollStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateMobileNumberAndCode :: Person -> SqlDB ()
updateMobileNumberAndCode person = do
  let personT = toTType person
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonMobileCountryCode =. val (TPerson.mobileCountryCode personT),
        PersonMobileNumberEncrypted =. val (TPerson.mobileNumberEncrypted personT),
        PersonMobileNumberHash =. val (TPerson.mobileNumberHash personT),
        PersonUnencryptedMobileNumber =. val (TPerson.unencryptedMobileNumber personT),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey person.id)

setIsNewFalse :: Id Person -> SqlDB ()
setIsNewFalse personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonIsNew =. val False,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

deleteById :: Id Person -> SqlDB ()
deleteById = Esq.deleteByKey @PersonT

updateAverageRating :: Id Person -> Centesimal -> SqlDB ()
updateAverageRating personId newAverageRating = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonRating =. val (Just newAverageRating),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

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
  (Transactionable m, MonadTime m, EsqDBReplicaFlow m r, EsqLocRepDBFlow m r) =>
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
    logDebug $ "GetNearestDriver - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicles:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
    return (linkArrayList driverLocs driverInfos vehicle drivers LatLong {..} mbVariant)
  logDebug $ "GetNearestDrivers Result:- " <> show (length res)
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
  (Transactionable m, MonadTime m, EsqLocRepDBFlow m r) =>
  Id Merchant ->
  Maybe Seconds ->
  LatLong ->
  Int ->
  m [DriverLocation]
getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry LatLong {..} radiusMeters = do
  now <- getCurrentTime
  runInLocReplica $
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
  (Transactionable m, EsqDBReplicaFlow m r) =>
  [DriverLocation] ->
  Bool ->
  Bool ->
  m [DriverInformation]
getDriverInfosWithCond driverLocs onlyNotOnRide onlyOnRide = do
  runInReplica $
    Esq.findAll $ do
      driverInfos <- from $ table @DriverInformationT
      where_ $
        driverInfos ^. DriverInformationDriverId `in_` valList personsKeys
          &&. ((Esq.isNothing (driverInfos ^. DriverInformationMode) &&. driverInfos ^. DriverInformationActive) ||. (not_ (Esq.isNothing (driverInfos ^. DriverInformationMode)) &&. (driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.SILENT) ||. driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.ONLINE))))
          &&. (if onlyNotOnRide then not_ (driverInfos ^. DriverInformationOnRide) else if onlyOnRide then driverInfos ^. DriverInformationOnRide else val True)
          &&. not_ (driverInfos ^. DriverInformationBlocked)
          &&. (driverInfos ^. DriverInformationSubscribed)
      return driverInfos
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromLocations driverLocs

getVehiclesWithCond ::
  (Transactionable m, EsqDBReplicaFlow m r) =>
  [DriverInformation] ->
  m [Vehicle]
getVehiclesWithCond driverInfo = do
  runInReplica $
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
  (Transactionable m, MonadTime m, EsqDBReplicaFlow m r, EsqLocRepDBFlow m r) =>
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
    logDebug $ "GetNearestDriversCurrentlyOnRide - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicle:- " <> show (length vehicles) <> " Drivers:- " <> show (length drivers) <> " Dquotes:- " <> show (length driverQuote) <> " BInfos:- " <> show (length bookingInfo) <> " BLocs:- " <> show (length bookingLocation)
    return (linkArrayListForOnRide driverQuote bookingInfo bookingLocation driverLocs driverInfos vehicles drivers LatLong {..} onRideRadius mbVariant)
  logDebug $ "GetNearestDriversCurrentlyOnRide Result:- " <> show (length res)
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

updateAlternateMobileNumberAndCode :: Person -> SqlDB ()
updateAlternateMobileNumberAndCode person = do
  now <- getCurrentTime
  let personT = toTType person
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonAlternateMobileNumberEncrypted =. val (TPerson.alternateMobileNumberEncrypted personT),
        PersonUnencryptedAlternateMobileNumber =. val (TPerson.unencryptedAlternateMobileNumber personT),
        PersonAlternateMobileNumberHash =. val (TPerson.alternateMobileNumberHash personT),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey person.id)

updateMediaId :: Id Person -> Maybe (Id MediaFile) -> SqlDB ()
updateMediaId driverId faceImageId =
  Esq.update $ \tbl -> do
    set
      tbl
      [PersonFaceImageId =. val (toKey <$> faceImageId)]
    where_ $ tbl ^. PersonTId ==. val (toKey $ cast driverId)
