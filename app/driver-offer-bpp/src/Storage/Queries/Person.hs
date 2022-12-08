{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.External.FCM.Types (FCMRecipientToken)
import qualified Beckn.External.FCM.Types as FCM
import Beckn.External.Maps as Maps
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.Version
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty
import Control.Applicative ((<|>))
import qualified Data.Maybe as Mb
import Domain.Types.DriverInformation
import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Vehicle as Vehicle
import GHC.Float (double2Int)
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person as TPerson
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
  ( Transactionable m,
    EncFlow m r
  ) =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId limitVal offsetVal mbVerified mbEnabled mbSearchPhone = do
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
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
        &&. maybe (val True) (\verified -> info ^. DriverInformationVerified ==. val verified) mbVerified
        &&. maybe (val True) (\enabled -> info ^. DriverInformationEnabled ==. val enabled) mbEnabled
        &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
    orderBy [asc (person ^. PersonFirstName)]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    pure (person, info, mbVeh)

findAllDriversByIdsFirstNameAsc ::
  (Transactionable m, Functor m) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc merchantId driverIds = fmap (map mkFullDriver) $
  Esq.findAll $ do
    (person :& driverLocation :& driverInfo :& vehicle) <-
      from baseFullPersonQuery
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonTId `in_` valList (map toKey driverIds)
        &&. person ^. PersonMerchantId ==. (val . toKey $ merchantId)
    orderBy [asc (person ^. PersonFirstName)]
    return (person, driverLocation, driverInfo, vehicle)

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

findByMobileNumber ::
  (Transactionable m) =>
  Text ->
  DbHash ->
  m (Maybe Person)
findByMobileNumber countryCode mobileNumberHash = do
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
    return person

findByIdentifier ::
  Transactionable m =>
  Text ->
  m (Maybe Person)
findByIdentifier identifier_ =
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonIdentifier ==. val (Just identifier_)
    return person

findByEmail ::
  Transactionable m =>
  Text ->
  m (Maybe Person)
findByEmail email_ =
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmail ==. val (Just email_)
    return person

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
            PersonClientVersion =. val mbBundleVersionText,
            PersonBundleVersion =. val mbClientVersionText
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

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Int,
    vehicle :: Vehicle,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

getNearestDrivers ::
  (Transactionable m, MonadTime m) =>
  Maybe Variant ->
  LatLong ->
  Integer ->
  Id Merchant ->
  Bool ->
  Maybe Seconds ->
  m [DriverPoolResult]
getNearestDrivers mbVariant LatLong {..} radiusMeters merchantId onlyNotOnRide mbDriverPositionInfoExpiry = do
  now <- getCurrentTime
  res <- Esq.findAll $ do
    withTable <- with $ do
      (person :& location :& driverInfo :& vehicle) <-
        from baseFullPersonQuery
      where_ $
        person ^. PersonRole ==. val Person.DRIVER
          &&. person ^. PersonMerchantId ==. val (toKey merchantId)
          &&. driverInfo ^. DriverInformationActive
          &&. (if onlyNotOnRide then not_ (driverInfo ^. DriverInformationOnRide) else val True)
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
          vehicle
        )
    (personId, mbDeviceToken, language, onRide, dist, dlat, dlon, vehicle) <- from withTable
    where_ $ dist <. val (fromIntegral radiusMeters)
    orderBy [asc dist]
    return (personId, mbDeviceToken, language, onRide, dist, vehicle, dlat, dlon)
  return $ makeDriverPoolResult <$> res
  where
    makeDriverPoolResult (personId, mbDeviceToken, mblang, onRide, dist, vehicle, dlat, dlon) =
      DriverPoolResult (cast personId) mbDeviceToken mblang onRide (double2Int dist) vehicle dlat dlon
