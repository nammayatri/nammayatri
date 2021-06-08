{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Storage.Queries.Person where

import App.Types
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
import Types.Error
import qualified Types.Storage.DB as DB
import Utils.Common
import Utils.PostgreSQLSimple (postgreSQLSimpleQuery)

getDbTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.PersonT))
getDbTable =
  DB.person . DB.transporterDb <$> getSchemaName

create :: Storage.Person -> Flow ()
create person = do
  dbTable <- getDbTable
  person' <- encrypt person
  DB.createOne dbTable (Storage.insertExpression person')

findPersonById ::
  Id Storage.Person -> Flow Storage.Person
findPersonById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= fromMaybeM PersonDoesNotExist
    >>= decrypt
  where
    predicate Storage.Person {..} = id ==. B.val_ pid

findPersonByIdAndRoleAndOrgId :: Id Storage.Person -> Storage.Role -> Id Org.Organization -> Flow (Maybe Storage.Person)
findPersonByIdAndRoleAndOrgId pid role_ orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      id ==. B.val_ pid
        &&. role ==. B.val_ role_
        &&. organizationId ==. B.val_ (Just orgId)

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Storage.Role] -> [Id Org.Organization] -> Flow [Storage.Person]
findAllWithLimitOffsetByOrgIds mlimit moffset roles orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
    >>= decrypt
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
  [Storage.Role] -> [Id Org.Organization] -> Flow [Storage.Person]
findAllByOrgIds roles orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
    >>= decrypt
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
  Text -> Text -> Flow (Maybe Storage.Person)
findByMobileNumber countryCode mobileNumber_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (mobileNumber ^. #hash) ==. B.val_ (Just $ evalDbHash mobileNumber_)

findByIdentifier ::
  Text -> Flow (Maybe Storage.Person)
findByIdentifier identifier_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      identifier ==. B.val_ (Just identifier_)

findByEmail ::
  Text -> Flow (Maybe Storage.Person)
findByEmail email_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      email ==. B.val_ (Just email_)

updateOrganizationIdAndMakeAdmin :: Id Storage.Person -> Id Org.Organization -> Flow ()
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
        [ firstName <-. B.val_ (Storage.firstName person),
          middleName <-. B.val_ (Storage.middleName person),
          lastName <-. B.val_ (Storage.lastName person),
          fullName <-. B.val_ (Storage.fullName person),
          role <-. B.val_ (Storage.role person),
          gender <-. B.val_ (Storage.gender person),
          email <-. B.val_ (Storage.email person),
          identifier <-. B.val_ (Storage.identifier person),
          rating <-. B.val_ (Storage.rating person),
          deviceToken <-. B.val_ (Storage.deviceToken person),
          udf1 <-. B.val_ (Storage.udf1 person),
          udf2 <-. B.val_ (Storage.udf2 person),
          organizationId <-. B.val_ (Storage.organizationId person),
          description <-. B.val_ (Storage.description person),
          locationId <-. B.val_ (Storage.locationId person),
          updatedAt <-. B.val_ n
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

updatePerson :: Id Storage.Person -> Bool -> Text -> Storage.IdentifierType -> Maybe Text -> Flow ()
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
  Id Storage.Person ->
  Storage.Status ->
  Bool ->
  Maybe FCM.FCMRecipientToken ->
  Flow ()
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

updateEntity :: Id Storage.Person -> Text -> Text -> Flow ()
updateEntity personId entityId entityType = do
  dbTable <- getDbTable
  let mEntityId =
        if null entityId
          then Nothing
          else Just entityId
      mEntityType =
        if null entityType
          then Nothing
          else Just entityType
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

findByEntityId :: Text -> Flow (Maybe Storage.Person)
findByEntityId entityId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      udf1 ==. B.val_ (Just entityId)

updateAverageRating :: Id Storage.Person -> Text -> Flow ()
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
            (driver ^. #locationId)
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
      B.customExpr_ distToPoint' lat lon (location ^. #point)
-}

getNearestDrivers ::
  LatLong ->
  Integer ->
  Id Org.Organization ->
  Vehicle.Variant ->
  Flow [(Id Driver, Double)]
getNearestDrivers LatLong {..} radius orgId variant =
  map (first Id)
    <$> postgreSQLSimpleQuery
      [sql|
        WITH a AS (
          SELECT
            person.id as id,
            location.point <-> public.ST_SetSRID(ST_Point(?, ?)::geography, 4326) as dist
          FROM atlas_transporter.person
          JOIN atlas_transporter.location
            ON person.location_id = location.id
          JOIN atlas_transporter.driver_information
            ON person.id = driver_information.driver_id
          JOIN atlas_transporter.vehicle
            ON person.udf1 = vehicle.id
          WHERE person.role = 'DRIVER'
            AND person.organization_id = ?
            AND driver_information.active
            AND NOT driver_information.on_ride
            AND vehicle.variant = ?
        )
        SELECT id, dist
        FROM a
        WHERE dist < ?
        ORDER BY dist ASC
      |]
      (lon, lat, getId orgId, show variant :: Text, radius)
