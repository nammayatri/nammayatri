{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.External.FCM.Types (FCMRecipientToken)
import qualified Beckn.External.Maps.HasCoordinates as GoogleMaps
import Beckn.External.Maps.Types (LatLong (..))
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Centesimal
import Beckn.Types.Id
import Beckn.Utils.Common hiding (Value)
import qualified Data.Maybe as Mb
import Domain.Types.DriverInformation
import Domain.Types.DriverLocation
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person
import Storage.Tabular.Ride
import Storage.Tabular.Vehicle

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
mkFullDriver (person, location, info, vehicle) = FullDriver {..}

findAllDriversFirstNameAsc ::
  (Transactionable m) =>
  Id Merchant ->
  m [FullDriver]
findAllDriversFirstNameAsc merchantId = fmap (map mkFullDriver) $
  Esq.findAll $ do
    (person :& location :& driverInfo :& vehicle) <- from baseFullPersonQuery
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
    orderBy [asc (person ^. PersonFirstName)]
    return (person, location, driverInfo, vehicle)

maxLimit :: Int
maxLimit = 20

defaultLimit :: Int
defaultLimit = 10

calcLimit :: Maybe Int -> Int
calcLimit = min maxLimit . fromMaybe defaultLimit

findAllDriversWithInfoAndVehicle ::
  ( Transactionable m,
    EncFlow m r
  ) =>
  Id Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId mbLimit mbOffset mbEnabled mbSearchPhone = do
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  Esq.findAll $ do
    let limitVal = fromIntegral $ calcLimit mbLimit
        offsetVal = maybe 0 fromIntegral mbOffset
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
      person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
        &&. maybe (val True) (\enabled -> info ^. DriverInformationEnabled ==. val enabled) mbEnabled
        &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
    orderBy [asc (person ^. PersonFirstName)]
    limit limitVal
    offset offsetVal
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
        &&. person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
    orderBy [asc (person ^. PersonFirstName)]
    return (person, driverLocation, driverInfo, vehicle)

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Vehicle,
    ridesCount :: Maybe Int
  }

mkDriverWithRidesCount :: (Person, DriverInformation, Vehicle, Maybe Int) -> DriverWithRidesCount
mkDriverWithRidesCount (person, info, vehicle, ridesCount) = DriverWithRidesCount {..}

mkDriverWithRidesCountQuery ::
  From (SqlExpr (Value PersonTId), SqlExpr (Value Int)) ->
  From
    ( Table PersonT
        :& Table DriverInformationT
        :& Table VehicleT
        :& (SqlExpr (Value (Maybe PersonTId)), SqlExpr (Value (Maybe Int)))
    )
mkDriverWithRidesCountQuery ridesCountAggQuery =
  table @PersonT
    `innerJoin` table @DriverInformationT
    `Esq.on` ( \(person :& driverInfo) ->
                 person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
             )
    `innerJoin` table @VehicleT
    `Esq.on` ( \(person :& _ :& vehicle) ->
                 person ^. PersonTId ==. vehicle ^. VehicleDriverId
             )
    `leftJoin` ridesCountAggQuery
    `Esq.on` ( \(person :& _ :& _ :& (mbPersonId, _mbRidesCount)) ->
                 just (person ^. PersonTId) ==. mbPersonId
             )

ridesCountAggTable :: SqlQuery (From (SqlExpr (Value PersonTId), SqlExpr (Value Int)))
ridesCountAggTable = with $ do
  ride <- from $ table @RideT
  where_ (not_ $ ride ^. RideStatus `in_` valList [Ride.NEW, Ride.CANCELLED])
  groupBy $ ride ^. RideDriverId
  pure (ride ^. RideDriverId, count @Int $ ride ^. RideId)

fetchFullDriverByMobileNumber :: (Transactionable m, EncFlow m r) => Id Merchant -> Text -> Text -> m (Maybe DriverWithRidesCount)
fetchFullDriverByMobileNumber merchantId mobileNumber mobileCountryCode = fmap (fmap mkDriverWithRidesCount) $ do
  mobileNumberDbHash <- getDbHash mobileNumber
  Esq.findOne $ do
    ridesCountAggQuery <- ridesCountAggTable
    person :& driverInfo :& vehicle :& (_, mbRidesCount) <-
      from $ mkDriverWithRidesCountQuery ridesCountAggQuery
    where_ $
      (person ^. PersonRole ==. val Person.DRIVER)
        &&. person ^. PersonMobileCountryCode ==. val (Just mobileCountryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
        &&. person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
    pure (person, driverInfo, vehicle, mbRidesCount)

fetchFullDriverInfoByVehNumber :: Transactionable m => Id Merchant -> Text -> m (Maybe DriverWithRidesCount)
fetchFullDriverInfoByVehNumber merchantId vehicleNumber = fmap (fmap mkDriverWithRidesCount) $
  Esq.findOne $ do
    ridesCountAggQuery <- ridesCountAggTable
    person :& driverInfo :& vehicle :& (_, mbRidesCount) <-
      from $ mkDriverWithRidesCountQuery ridesCountAggQuery
    where_ $
      (person ^. PersonRole ==. val Person.DRIVER)
        &&. vehicle ^. VehicleRegistrationNo ==. val vehicleNumber
        &&. person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
    pure (person, driverInfo, vehicle, mbRidesCount)

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
        &&. person ^. PersonMerchantId ==. val (Just $ toKey merchantId)
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
        &&. person ^. PersonMerchantId ==. val (Just $ toKey merchantId)
    return person

findAdminsByMerchantId :: Transactionable m => Id Merchant -> m [Person]
findAdminsByMerchantId merchantId =
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMerchantId ==. val (Just (toKey merchantId))
        &&. person ^. PersonRole ==. val Person.ADMIN
    return person

findByMobileNumber ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByMobileNumber countryCode mobileNumber_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
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
      [ PersonMerchantId =. val (Just $ toKey merchantId),
        PersonRole =. val Person.ADMIN,
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
        PersonDeviceToken =. val (person.deviceToken),
        PersonMerchantId =. val (toKey <$> person.merchantId),
        PersonDescription =. val (person.description),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

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
    distanceToPickup :: HighPrecMeters,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, GoogleMaps.HasCoordinates)

getNearestDrivers ::
  (Transactionable m, MonadTime m) =>
  LatLong ->
  Integer ->
  Id Merchant ->
  Maybe Vehicle.Variant ->
  SFP.FareProductType ->
  Maybe Seconds ->
  m [NearestDriversResult]
getNearestDrivers LatLong {..} radius merchantId mbPoolVariant fareProductType mbDriverPositionInfoExpiry = do
  let isRental = fareProductType == SFP.RENTAL
  now <- getCurrentTime
  res <- Esq.findAll $ do
    withTable <- with $ do
      (person :& location :& driverInfo :& vehicle) <- from baseFullPersonQuery
      where_ $
        person ^. PersonRole ==. val Person.DRIVER
          &&. person ^. PersonMerchantId ==. val (Just $ toKey merchantId)
          &&. driverInfo ^. DriverInformationActive
          &&. driverInfo ^. DriverInformationOptForRental >=. val isRental
          &&. not_ (driverInfo ^. DriverInformationOnRide)
          &&. ( Esq.isNothing (val mbPoolVariant) ||. just (vehicle ^. VehicleVariant) ==. val mbPoolVariant -- when mbVariant = Nothing, we use all variants, is it correct?
                  ||. ( case mbPoolVariant of
                          Just SEDAN ->
                            driverInfo ^. DriverInformationCanDowngradeToSedan ==. val True
                              &&. vehicle ^. VehicleVariant ==. val SUV
                          Just HATCHBACK -> driverInfo ^. DriverInformationCanDowngradeToHatchback ==. val True
                          _ -> val False
                      )
              )
          &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                  ||. (location ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
              )
      return
        ( person ^. PersonTId,
          location ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon),
          vehicle ^. VehicleVariant,
          driverInfo ^. DriverInformationCanDowngradeToSedan,
          driverInfo ^. DriverInformationCanDowngradeToHatchback,
          location ^. DriverLocationLat,
          location ^. DriverLocationLon
        )
    (personId, dist, vehicleVariant, canDowngradeToSedan, canDowngradeToHatchback, dlat, dlon) <- from withTable
    where_ $ dist <. val (fromIntegral radius)
    orderBy [asc dist]
    return (personId, dist, vehicleVariant, canDowngradeToSedan, canDowngradeToHatchback, dlat, dlon)
  return (makeDriverPoolResults =<< res)
  where
    makeDriverPoolResults :: (Id Person, Double, Variant, Bool, Bool, Double, Double) -> [NearestDriversResult]
    makeDriverPoolResults (personId, dist, vehicleVariant, canDowngradeToSedan, canDowngradeToHatchback, dlat, dlon) = do
      case mbPoolVariant of
        Nothing -> do
          let suvResult = getResult SUV $ vehicleVariant == SUV
              sedanResult = getResult SEDAN $ vehicleVariant == SEDAN || (vehicleVariant == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ vehicleVariant == HATCHBACK || canDowngradeToHatchback
          suvResult <> sedanResult <> hatchbackResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestDriversResult (cast personId) (HighPrecMeters $ realToFrac dist) var dlat dlon | cond]
