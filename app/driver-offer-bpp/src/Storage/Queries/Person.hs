{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.External.FCM.Types (FCMRecipientToken)
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Utils.GenericPretty
import qualified Data.Maybe as Mb
import Domain.Types.Organization
import Domain.Types.Person as Person
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person
import Storage.Tabular.Vehicle as Vehicle
import Types.App (Driver)
import Utils.Common

create :: Person -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Person)
findById = Esq.findById

findByIdAndRoleAndOrgId ::
  Transactionable m =>
  Id Person ->
  Person.Role ->
  Id Organization ->
  m (Maybe Person)
findByIdAndRoleAndOrgId pid role_ orgId =
  Esq.findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonTId ==. val (toKey pid)
        &&. person ^. PersonRole ==. val role_
        &&. person ^. PersonOrganizationId ==. val (Just $ toKey orgId)
    return person

findAllByOrgId ::
  Transactionable m =>
  [Person.Role] ->
  Id Organization ->
  m [Person]
findAllByOrgId roles orgId =
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      (person ^. PersonRole `in_` valList roles ||. val (null roles))
        &&. person ^. PersonOrganizationId ==. val (Just $ toKey orgId)
    return person

findAdminsByOrgId :: Transactionable m => Id Organization -> m [Person]
findAdminsByOrgId orgId =
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonOrganizationId ==. val (Just (toKey orgId))
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

updateOrganizationIdAndMakeAdmin :: Id Person -> Id Organization -> SqlDB ()
updateOrganizationIdAndMakeAdmin personId orgId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonOrganizationId =. val (Just $ toKey orgId),
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
        PersonUdf1 =. val (person.udf1),
        PersonUdf2 =. val (person.udf2),
        PersonOrganizationId =. val (toKey <$> person.organizationId),
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

updateVehicle :: Id Person -> Maybe (Id Vehicle) -> SqlDB ()
updateVehicle personId mbVehicleId = do
  let (mEntityId, mEntityType) = case mbVehicleId of
        Just vehicleId -> (Just (getId vehicleId), Just "VEHICLE")
        Nothing -> (Nothing, Nothing)
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonUdf1 =. val mEntityId,
        PersonUdf2 =. val mEntityType,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

findByVehicleId :: Transactionable m => Id Vehicle -> m (Maybe Person)
findByVehicleId (Id vehicleId) =
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonUdf1 ==. val (Just vehicleId)
    return person

updateAverageRating :: Id Person -> Double -> SqlDB ()
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
    distanceToDriver :: Double,
    vehicle :: Vehicle,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, PrettyShow, Show)

instance GoogleMaps.HasCoordinates DriverPoolResult where
  getCoordinates dpRes = LatLong dpRes.lat dpRes.lon

getNearestDrivers ::
  (Transactionable m, HasFlowEnv m r '["driverPositionInfoExpiry" ::: Maybe Seconds]) =>
  LatLong ->
  Integer ->
  Id Organization ->
  m [DriverPoolResult]
getNearestDrivers LatLong {..} radiusMeters orgId = do
  mbDriverPositionInfoExpiry <- asks (.driverPositionInfoExpiry)
  now <- getCurrentTime
  res <- Esq.findAll $ do
    withTable <- with $ do
      (person :& location :& driverInfo :& vehicle) <-
        from $
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
                         (person ^. PersonUdf1) ==. just (castString $ vehicle ^. VehicleId)
                     )

      where_ $
        person ^. PersonRole ==. val Person.DRIVER
          &&. person ^. PersonOrganizationId ==. val (Just $ toKey orgId)
          &&. driverInfo ^. DriverInformationActive
          &&. not_ (driverInfo ^. DriverInformationOnRide)
          &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                  ||. (location ^. DriverLocationUpdatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
              )
      return
        ( person ^. PersonTId,
          location ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon),
          location ^. DriverLocationLat,
          location ^. DriverLocationLon,
          vehicle
        )
    (personId, dist, dlat, dlon, vehicleVariant) <- from withTable
    where_ $ dist <. val (fromIntegral radiusMeters)
    orderBy [asc dist]
    return (personId, dist, vehicleVariant, dlat, dlon)
  return $ makeDriverPoolResult <$> res
  where
    makeDriverPoolResult (personId, dist, vehicleVariant, dlat, dlon) =
      DriverPoolResult (cast personId) dist vehicleVariant dlat dlon

setRegisteredTrue :: Id Person -> SqlDB ()
setRegisteredTrue personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonRegistered =. val True,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

