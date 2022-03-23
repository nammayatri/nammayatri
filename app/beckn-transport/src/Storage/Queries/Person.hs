{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Types.Schema
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Storage
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common
import Utils.PostgreSQLSimple (postgreSQLSimpleQuery)

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.PersonT))
getDbTable =
  DB.person . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Person -> m ()
createFlow = DB.runSqlDB . create

create :: Storage.Person -> DB.SqlDB ()
create person = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue person)

findPersonById ::
  DBFlow m r =>
  Id Storage.Person ->
  m (Maybe Storage.Person)
findPersonById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} = id ==. B.val_ pid

findPersonByIdAndRoleAndOrgId ::
  DBFlow m r =>
  Id Storage.Person ->
  Storage.Role ->
  Id Org.Organization ->
  m (Maybe Storage.Person)
findPersonByIdAndRoleAndOrgId pid role_ orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      id ==. B.val_ pid
        &&. role ==. B.val_ role_
        &&. organizationId ==. B.val_ (Just orgId)

findAllByOrgId ::
  DBFlow m r =>
  [Storage.Role] ->
  Id Org.Organization ->
  m [Storage.Person]
findAllByOrgId roles orgId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          organizationId B.==. B.val_ (Just orgId)
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findByMobileNumber ::
  (DBFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Storage.Person)
findByMobileNumber countryCode mobileNumber_ = do
  dbTable <- getDbTable
  mobileNumberDbHash <- getDbHash mobileNumber_
  DB.findOne dbTable (predicate mobileNumberDbHash)
  where
    predicate mobileNumberDbHash Storage.Person {..} =
      mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (mobileNumber.hash) ==. B.val_ (Just mobileNumberDbHash)

findByIdentifier ::
  DBFlow m r =>
  Text ->
  m (Maybe Storage.Person)
findByIdentifier identifier_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      identifier ==. B.val_ (Just identifier_)

findByEmail ::
  DBFlow m r =>
  Text ->
  m (Maybe Storage.Person)
findByEmail email_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      email ==. B.val_ (Just email_)

updateOrganizationIdAndMakeAdmin :: DBFlow m r => Id Storage.Person -> Id Org.Organization -> m ()
updateOrganizationIdAndMakeAdmin personId orgId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause orgId now) (predicate personId)
  where
    setClause sOrgId n Storage.Person {..} =
      mconcat
        [ organizationId <-. B.val_ (Just sOrgId),
          role <-. B.val_ Storage.ADMIN,
          updatedAt <-. B.val_ n
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

updatePersonRec :: Id Storage.Person -> Storage.PersonT Identity -> DB.SqlDB ()
updatePersonRec personId person' = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause person' now) (predicate personId)
  where
    setClause person n Storage.Person {..} =
      mconcat
        [ firstName <-. B.val_ (person.firstName),
          middleName <-. B.val_ (person.middleName),
          lastName <-. B.val_ (person.lastName),
          fullName <-. B.val_ (person.fullName),
          role <-. B.val_ (person.role),
          gender <-. B.val_ (person.gender),
          email <-. B.val_ (person.email),
          identifier <-. B.val_ (person.identifier),
          rating <-. B.val_ (person.rating),
          deviceToken <-. B.val_ (person.deviceToken),
          udf1 <-. B.val_ (person.udf1),
          udf2 <-. B.val_ (person.udf2),
          organizationId <-. B.val_ (person.organizationId),
          description <-. B.val_ (person.description),
          updatedAt <-. B.val_ n
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

updateDeviceToken :: Id Storage.Person -> Maybe FCMRecipientToken -> DB.SqlDB ()
updateDeviceToken personId mbDeviceToken = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause now mbDeviceToken) (predicate personId)
  where
    setClause currTime mbDeviceToken_ Storage.Person {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          deviceToken <-. B.val_ mbDeviceToken_
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

setIsNewFalse :: Id Storage.Person -> DB.SqlDB ()
setIsNewFalse personId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause now) (predicate personId)
  where
    setClause currTime Storage.Person {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          isNew <-. B.val_ False
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

deleteById :: Id Storage.Person -> DB.SqlDB ()
deleteById personId = do
  dbTable <- getDbTable
  DB.delete' dbTable (predicate personId)
  where
    predicate pid Storage.Person {..} = id ==. B.val_ pid

updateVehicleFlow :: DBFlow m r => Id Storage.Person -> Maybe (Id Vehicle.Vehicle) -> m ()
updateVehicleFlow personId = DB.runSqlDB . updateVehicle personId

updateVehicle :: Id Storage.Person -> Maybe (Id Vehicle.Vehicle) -> DB.SqlDB ()
updateVehicle personId mbVehicleId = do
  dbTable <- getDbTable
  let (mEntityId, mEntityType) = case mbVehicleId of
        Just vehicleId -> (Just (getId vehicleId), Just "VEHICLE")
        Nothing -> (Nothing, Nothing)
  DB.update'
    dbTable
    (setClause mEntityId mEntityType)
    (predicate personId)
  where
    setClause mEntityId mEntityType Storage.Person {..} =
      mconcat
        [ udf1 <-. B.val_ mEntityId,
          udf2 <-. B.val_ mEntityType
        ]
    predicate pId Storage.Person {..} = id ==. B.val_ pId

findByVehicleId :: DBFlow m r => Id Vehicle.Vehicle -> m (Maybe Storage.Person)
findByVehicleId (Id vehicleId) = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      udf1 ==. B.val_ (Just vehicleId)

updateAverageRating :: DBFlow m r => Id Storage.Person -> Double -> m ()
updateAverageRating personId newAverageRating = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause newAverageRating now) (predicate personId)
  where
    setClause rating_ now Storage.Person {..} =
      mconcat
        [ rating <-. B.val_ (Just rating_),
          updatedAt <-. B.val_ now
        ]
    predicate pId Storage.Person {..} = id ==. B.val_ pId

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    distanceToDriver :: Double,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, FromRow)

getNearestDrivers ::
  (DBFlow m r, HasFlowEnv m r '["driverPositionInfoExpiry" ::: Maybe Seconds]) =>
  LatLong ->
  Integer ->
  Id Org.Organization ->
  Maybe Vehicle.Variant ->
  m [DriverPoolResult]
getNearestDrivers LatLong {..} radius orgId variant = do
  mbDriverPositionInfoExpiry <- asks (.driverPositionInfoExpiry)
  postgreSQLSimpleQuery
    [sql|
        WITH a AS (
          SELECT
            person.id AS id,
            driver_location.point <-> public.ST_SetSRID(ST_Point(?, ?)::geography, 4326) AS dist,
            vehicle.variant AS vehicle_variant,
            driver_location.lat AS lat,
            driver_location.lon AS lon
          FROM atlas_transporter.person
          JOIN atlas_transporter.driver_location
            ON person.id = driver_location.driver_id
          JOIN atlas_transporter.driver_information
            ON person.id = driver_information.driver_id
          JOIN atlas_transporter.vehicle
            ON person.udf1 = vehicle.id
          WHERE person.role = 'DRIVER'
            AND person.organization_id = ?
            AND driver_information.active
            AND NOT driver_information.on_ride
            AND COALESCE(vehicle.variant = ?, true)
            AND (? OR driver_location.updated_at + interval '? seconds' >= now())
        )
        SELECT id, dist, vehicle_variant, lat, lon
        FROM a
        WHERE dist < ?
        ORDER BY dist ASC
      |]
    (lon, lat, getId orgId, show @Text <$> variant, isNothing mbDriverPositionInfoExpiry, maybe 0 getSeconds mbDriverPositionInfoExpiry, radius)
