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
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.Person as Storage
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import Database.PostgreSQL.Simple.SqlQQ (sql)
import EulerHS.Prelude hiding (id)
import Types.API.Location (LatLong (..))
import Types.App
import qualified Types.Storage.DB as DB
import Utils.Common
import Utils.PostgreSQLSimple (postgreSQLSimpleQuery)

getDbTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.PersonT))
getDbTable =
  DB.person . DB.transporterDb <$> getSchemaName

create :: DBFlow m r => Storage.Person -> m ()
create person = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression person)

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

findAllWithLimitOffsetByOrgIds ::
  DBFlow m r =>
  Maybe Integer ->
  Maybe Integer ->
  [Storage.Role] ->
  [Id Org.Organization] ->
  m [Storage.Person]
findAllWithLimitOffsetByOrgIds mlimit moffset roles orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Person {..} = B.desc_ createdAt
    limit = fromMaybe 100 mlimit
    offset = fromMaybe 0 moffset
    predicate Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal orgIds
        ]

findAllByOrgIds ::
  DBFlow m r =>
  [Storage.Role] ->
  [Id Org.Organization] ->
  m [Storage.Person]
findAllByOrgIds roles orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal orgIds
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findByMobileNumber ::
  DBFlow m r =>
  Text ->
  Text ->
  m (Maybe Storage.Person)
findByMobileNumber countryCode mobileNumber_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (mobileNumber.hash) ==. B.val_ (Just $ evalDbHash mobileNumber_)

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
          locationId <-. B.val_ (person.locationId),
          updatedAt <-. B.val_ n
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

updatePerson :: (DBFlow m r, EncFlow m r) => Id Storage.Person -> Bool -> Text -> Storage.IdentifierType -> Maybe Text -> m ()
updatePerson personId verified_ identifier_ identifierType_ mobileNumber_ = do
  dbTable <- getDbTable
  now <- getCurrentTime
  mobileNumber' <- encrypt mobileNumber_
  DB.update
    dbTable
    (setClause identifier_ identifierType_ mobileNumber' verified_ now)
    (predicate personId)
  where
    setClause i it mn v n Storage.Person {..} =
      mconcat
        [ identifier <-. B.val_ (Just i),
          identifierType <-. B.val_ it,
          mobileNumber <-. B.val_ mn,
          verified <-. B.val_ v,
          updatedAt <-. B.val_ n
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

update ::
  DBFlow m r =>
  Id Storage.Person ->
  Storage.Status ->
  Bool ->
  Maybe FCM.FCMRecipientToken ->
  m ()
update personId status_ verified_ deviceTokenM = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ verified_ currTime deviceTokenM)
    (predicate personId)
  where
    setClause sStatus sVerified currTime deviceToken_ Storage.Person {..} =
      mconcat
        [ status <-. B.val_ sStatus,
          updatedAt <-. B.val_ currTime,
          verified <-. B.val_ sVerified,
          deviceToken <-. B.val_ deviceToken_
        ]
    predicate pid Storage.Person {..} = id ==. B.val_ pid

deleteById :: Id Storage.Person -> DB.SqlDB ()
deleteById personId = do
  dbTable <- getDbTable
  DB.delete' dbTable (predicate personId)
  where
    predicate pid Storage.Person {..} = id ==. B.val_ pid

updateVehicle :: DBFlow m r => Id Storage.Person -> Maybe (Id Vehicle.Vehicle) -> m ()
updateVehicle personId mbVehicleId = do
  dbTable <- getDbTable
  let (mEntityId, mEntityType) = case mbVehicleId of
        Just vehicleId -> (Just (getId vehicleId), Just "VEHICLE")
        Nothing -> (Nothing, Nothing)
  DB.update
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

updateAverageRating :: DBFlow m r => Id Storage.Person -> Text -> m ()
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

{-
-- This is an attempt to implement custom postgis query using beam.
-- Eventually it was implemented with postgis-simple (see below)
-- and this version was left for proper beam implementation in the future:
getNearestDrivers
  :: LatLong
  -> Double
  -> Maybe OrganizationId
  -> Flow [(Storage.Person, Double)]
getNearestDrivers point' radius' orgId' = do
  personTable <- getDbTable
  locationTable <- Location.getDbTable
  let orgId = getId <$> orgId'
  DB.findAllByJoinWithoutLimits orderBy
    (query personTable locationTable point' radius' orgId)
    >>= checkDBError
    >>= decryptPerson
  where
    decryptPerson
      :: [(Storage.PersonT Identity, Double)]
      -> Flow [(Storage.Person, Double)]
    decryptPerson = traverse $ \(encper, dist) -> (, dist) <$> decrypt encper
    query personTable locationTable point radius orgId = do
      driver <- B.all_ personTable
      location <- B.join_ locationTable $
        \location ->
          B.maybe_
            (pure False)
            ( (B.primaryKey location ==.)
                . Storage.LocationPrimaryKey
                . fmap Id
            )
            (driver.locationId)
      dist <- distToPoint point location
      driver' <- B.filter_ (predicate orgId radius dist) (pure driver)
      return (driver', dist)
    orderBy (_, dist) = B.asc_ dist
    predicate orgId radius dist Storage.Person {..} =
      role ==. B.val_ Storage.DRIVER
        &&. organizationId ==. B.val_ orgId
        &&. dist <. B.val_ radius

    distToPoint' :: (Monoid a, IsString a) => a -> a -> a -> a
    distToPoint' lat lon point =
      point <> " <-> ST_Point(" <> lat <> ", " <> lon <> ")::geometry"
    distToPoint LatLong {..} location =
      B.customExpr_ distToPoint' lat lon (location.point)
-}

getNearestDrivers ::
  DBFlow m r =>
  LatLong ->
  Integer ->
  Id Org.Organization ->
  Vehicle.Variant ->
  m [(Id Driver, Double)]
getNearestDrivers LatLong {..} radius orgId variant =
  map (first Id)
    <$> postgreSQLSimpleQuery
      [sql|
        WITH a AS (
          SELECT
            person."id" as id,
            location."point" <-> public.ST_SetSRID(ST_Point(?, ?)::geography, 4326) as dist
          FROM atlas_transporter."person"
          JOIN atlas_transporter."location"
            ON person."location_id" = location."id"
          JOIN atlas_transporter."driver_information"
            ON person."id" = driver_information."driver_id"
          JOIN atlas_transporter."vehicle"
            ON person."udf1" = vehicle."id"
          WHERE person."role" = 'DRIVER'
            AND person."organization_id" = ?
            AND driver_information."active"
            AND NOT driver_information."on_ride"
            AND vehicle."variant" = ?
        )
        SELECT id, dist
        FROM a
        WHERE dist < ?
        ORDER BY dist ASC
      |]
      (lon, lat, getId orgId, show variant :: Text, radius)
