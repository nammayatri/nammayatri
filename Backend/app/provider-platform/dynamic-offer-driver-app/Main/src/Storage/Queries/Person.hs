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
import qualified Data.Maybe as Mb
import Domain.Types.DriverInformation
import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Ride as Ride
import Domain.Types.Vehicle as Vehicle
import Kernel.External.Encryption
import Kernel.External.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.FCM.Types as FCM
import Kernel.External.Maps as Maps
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.GenericPretty
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person as TPerson
import Storage.Tabular.Ride
import Storage.Tabular.Vehicle as Vehicle

baseFullPersonQuery ::
  From
    ( Table PersonT
        :& Table DriverLocationT
        :& Table DriverInformationT
        :& Table VehicleT
    )
baseFullPersonQuery =
  table @PersonT
    `innerJoin` table @DriverLocationT
    `Esq.on` ( \(person :& location) ->
                 person ^. PersonTId ==. location ^. DriverLocationDriverId
             )
    `innerJoin` table @DriverInformationT
    `Esq.on` ( \(person :& _ :& driverInfo) ->
                 person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
             )
    `innerJoin` table @VehicleT
    `Esq.on` ( \(person :& _ :& _ :& vehicle) ->
                 person ^. PersonTId ==. vehicle ^. VehicleDriverId
             )

create :: Person -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Proxy ma ->
  Id Person ->
  m (Maybe Person)
findById _ = Esq.findById @m @ma

data FullDriver = FullDriver
  { person :: Person,
    location :: DriverLocation,
    info :: DriverInformation,
    vehicle :: Vehicle
  }

mkFullDriver :: (Person, DriverLocation, DriverInformation, Vehicle) -> FullDriver
mkFullDriver (p, l, i, v) = FullDriver p l i v

findAllDriversWithInfoAndVehicle ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  Proxy ma ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId limitVal offsetVal mbVerified mbEnabled mbBlocked mbSearchPhoneDBHash _ = do
  Esq.findAll @m @ma $ do
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
        &&. maybe (val True) (\verified -> info ^. DriverInformationVerified ==. val verified) mbVerified
        &&. maybe (val True) (\enabled -> info ^. DriverInformationEnabled ==. val enabled) mbEnabled
        &&. maybe (val True) (\blocked -> info ^. DriverInformationBlocked ==. val blocked) mbBlocked
        &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
    orderBy [asc (person ^. PersonFirstName)]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    pure (person, info, mbVeh)

countDrivers :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m Int
countDrivers merchantId _ =
  mkCount <$> do
    Esq.findAll @m @ma $ do
      person <- from $ table @PersonT
      where_ $
        person ^. PersonMerchantId ==. val (toKey merchantId)
          &&. person ^. PersonRole ==. val Person.DRIVER
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

findAllDriversByIdsFirstNameAsc ::
  forall m ma.
  (Transactionable ma m, Functor m) =>
  Id Merchant ->
  [Id Person] ->
  Proxy ma ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc merchantId driverIds _ = fmap (map mkFullDriver) $
  Esq.findAll @m @ma $ do
    (person :& driverLocation :& driverInfo :& vehicle) <-
      from baseFullPersonQuery
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonTId `in_` valList (map toKey driverIds)
        &&. person ^. PersonMerchantId ==. (val . toKey $ merchantId)
    orderBy [asc (person ^. PersonFirstName)]
    return (person, driverLocation, driverInfo, vehicle)

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int
  }

mkDriverWithRidesCount :: (Person, DriverInformation, Maybe Vehicle, Maybe Int) -> DriverWithRidesCount
mkDriverWithRidesCount (person, info, vehicle, ridesCount) = DriverWithRidesCount {..}

ridesCountAggTable :: SqlQuery (From (SqlExpr (Value PersonTId), SqlExpr (Value Int)))
ridesCountAggTable = with $ do
  ride <- from $ table @RideT
  where_ (not_ $ ride ^. RideStatus `in_` valList [Ride.NEW, Ride.CANCELLED])
  groupBy $ ride ^. RideDriverId
  pure (ride ^. RideDriverId, count @Int $ ride ^. RideId)

fetchDriverInfoWithRidesCount ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Maybe (DbHash, Text) ->
  Maybe Text ->
  Maybe DbHash ->
  Maybe DbHash ->
  Proxy ma ->
  m (Maybe DriverWithRidesCount)
fetchDriverInfoWithRidesCount merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash _ = fmap (fmap mkDriverWithRidesCount) $ do
  Esq.findOne @m @ma $ do
    ridesCountAggQuery <- ridesCountAggTable
    person :& driverInfo :& mbVehicle :& mbDriverLicense :& _mbRcAssoc :& mbRegCert :& (_, mbRidesCount) <-
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
                         just (person ^. PersonTId) ==. mbRcAssoc ?. DriverRCAssociationDriverId
                   )
          `leftJoin` table @VehicleRegistrationCertificateT
          `Esq.on` ( \(_ :& _ :& _ :& _ :& mbRcAssoc :& mbRegCert) ->
                       joinOnlyWhenJust mbRcNumberHash $
                         mbRcAssoc ?. DriverRCAssociationRcId ==. mbRegCert ?. VehicleRegistrationCertificateTId
                   )
          `leftJoin` ridesCountAggQuery
          `Esq.on` ( \(person :& _ :& _ :& _ :& _ :& _ :& (mbPersonId, _mbRidesCount)) ->
                       just (person ^. PersonTId) ==. mbPersonId
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
    pure (person, driverInfo, mbVehicle, mbRidesCount)
  where
    -- used only for dl and rc entites, because they are not required for final result, only for filters
    joinOnlyWhenJust mbFilter cond = maybe (val False) (const cond) mbFilter

findByIdAndRoleAndMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Person.Role ->
  Id Merchant ->
  Proxy ma ->
  m (Maybe Person)
findByIdAndRoleAndMerchantId pid role_ merchantId _ =
  Esq.findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonTId ==. val (toKey pid)
        &&. person ^. PersonRole ==. val role_
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findAllByMerchantId ::
  forall m ma.
  Transactionable ma m =>
  [Person.Role] ->
  Id Merchant ->
  Proxy ma ->
  m [Person]
findAllByMerchantId roles merchantId _ =
  Esq.findAll @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      (person ^. PersonRole `in_` valList roles ||. val (null roles))
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findAdminsByMerchantId :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m [Person]
findAdminsByMerchantId merchantId _ =
  Esq.findAll @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. person ^. PersonRole ==. val Person.ADMIN
    return person

findByMobileNumber ::
  forall m ma.
  Transactionable ma m =>
  Text ->
  DbHash ->
  Proxy ma ->
  m (Maybe Person)
findByMobileNumber countryCode mobileNumberHash _ = do
  findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
    return person

findByIdentifier ::
  forall m ma.
  Transactionable ma m =>
  Text ->
  Proxy ma ->
  m (Maybe Person)
findByIdentifier identifier_ _ =
  findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonIdentifier ==. val (Just identifier_)
    return person

findByEmail ::
  forall m ma.
  Transactionable ma m =>
  Text ->
  Proxy ma ->
  m (Maybe Person)
findByEmail email_ _ =
  findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmail ==. val (Just email_)
    return person

updateMerchantIdAndMakeAdmin :: Id Person -> Id Merchant -> SqlDB m ()
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

updateName :: Id Person -> Text -> SqlDB m ()
updateName personId name = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFirstName =. val name,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonRec :: Id Person -> Person -> SqlDB m ()
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

updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB m ()
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

updateDeviceToken :: Id Person -> Maybe FCMRecipientToken -> SqlDB m ()
updateDeviceToken personId mbDeviceToken = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonDeviceToken =. val mbDeviceToken,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB m ()
updateWhatsappNotificationEnrollStatus personId enrollStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateMobileNumberAndCode :: Person -> SqlDB m ()
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

setIsNewFalse :: Id Person -> SqlDB m ()
setIsNewFalse personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonIsNew =. val False,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

deleteById :: Id Person -> SqlDB m ()
deleteById = Esq.deleteByKey @PersonT

updateAverageRating :: Id Person -> Centesimal -> SqlDB m ()
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
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

getNearestDrivers ::
  forall m ma.
  (Transactionable ma m, MonadTime m) =>
  Maybe Variant ->
  LatLong ->
  Int ->
  Id Merchant ->
  Bool ->
  Maybe Seconds ->
  Proxy ma ->
  m [NearestDriversResult]
getNearestDrivers mbVariant LatLong {..} radiusMeters merchantId onlyNotOnRide mbDriverPositionInfoExpiry _ = do
  now <- getCurrentTime
  res <- Esq.findAll @m @ma $ do
    withTable <- with $ do
      (person :& location :& driverInfo :& vehicle) <-
        from baseFullPersonQuery
      where_ $
        person ^. PersonRole ==. val Person.DRIVER
          &&. person ^. PersonMerchantId ==. val (toKey merchantId)
          &&. driverInfo ^. DriverInformationActive
          &&. (if onlyNotOnRide then not_ (driverInfo ^. DriverInformationOnRide) else val True)
          &&. not_ (driverInfo ^. DriverInformationBlocked)
          &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                  ||. (location ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
              )
          &&. whenJust_ mbVariant (\var -> vehicle ^. VehicleVariant ==. val var)
      return
        ( person ^. PersonTId,
          person ^. PersonDeviceToken,
          person ^. PersonLanguage,
          driverInfo ^. DriverInformationOnRide,
          location ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon),
          location ^. DriverLocationLat,
          location ^. DriverLocationLon,
          vehicle ^. VehicleVariant
        )
    (personId, mbDeviceToken, language, onRide, dist, dlat, dlon, variant) <- from withTable
    where_ $ dist <. val (fromIntegral radiusMeters)
    orderBy [asc dist]
    return (personId, mbDeviceToken, language, onRide, dist, variant, dlat, dlon)
  return $ makeNearestDriversResult <$> res
  where
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, dist :: Double, variant, dlat, dlon) =
      NearestDriversResult (cast personId) mbDeviceToken mblang onRide (roundToIntegral dist) variant dlat dlon
