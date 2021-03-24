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
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.Person as Storage
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import Database.PostgreSQL.Simple.SqlQQ (sql)
import EulerHS.Prelude hiding (id)
import Types.API.Location (LatLong (..))
import Types.App
import Types.Error
import qualified Types.Storage.DB as DB
import Utils.PostgreSQLSimple (postgreSQLSimpleQuery)

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.PersonT))
getDbTable =
  DB._person . DB.transporterDb <$> getSchemaName

create :: Storage.Person -> Flow ()
create person = do
  dbTable <- getDbTable
  person' <- encrypt person
  DB.createOne dbTable (Storage.insertExpression person')
    >>= either throwDBError pure

findPersonById ::
  Id Storage.Person -> Flow Storage.Person
findPersonById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
    >>= fromMaybeM PersonDoesNotExist
    >>= decrypt
  where
    predicate Storage.Person {..} = _id ==. B.val_ id

findPersonByIdAndRoleAndOrgId :: Id Storage.Person -> Storage.Role -> Text -> Flow (Maybe Storage.Person)
findPersonByIdAndRoleAndOrgId id role orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _id ==. B.val_ id
        &&. _role ==. B.val_ role
        &&. _organizationId ==. B.val_ (Just orgId)

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Storage.Role] -> [Text] -> Flow [Storage.Person]
findAllWithLimitOffsetByOrgIds mlimit moffset roles orgIds = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= either throwDBError pure
    >>= decrypt
  where
    orderByDesc Storage.Person {..} = B.desc_ _createdAt
    limit = fromMaybe 100 mlimit
    offset = fromMaybe 0 moffset
    predicate Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          _organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal orgIds
        ]

findAllByOrgIds ::
  [Storage.Role] -> [Text] -> Flow [Storage.Person]
findAllByOrgIds roles orgIds = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          _organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal orgIds
        ]

findAllByRoles ::
  [Storage.Role] -> Flow [Storage.Person]
findAllByRoles roles = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [_role `B.in_` (B.val_ <$> roles) ||. complementVal roles]

findAllActiveDrivers :: Flow [Storage.Person]
findAllActiveDrivers = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
    >>= decrypt
  where
    predicate Storage.Person {..} =
      foldr
        (&&.)
        (B.val_ True)
        [ _role ==. B.val_ Storage.DRIVER,
          _status ==. B.val_ Storage.ACTIVE
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findByMobileNumber ::
  Text -> Text -> Flow (Maybe Storage.Person)
findByMobileNumber countryCode mobileNumber = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (_mobileNumber ^. #_hash) ==. B.val_ (Just $ evalDbHash mobileNumber)

findByIdentifier ::
  Text -> Flow (Maybe Storage.Person)
findByIdentifier identifier = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _identifier ==. B.val_ (Just identifier)

findByEmail ::
  Text -> Flow (Maybe Storage.Person)
findByEmail email = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _email ==. B.val_ (Just email)

updateOrganizationIdAndMakeAdmin :: Id Storage.Person -> Text -> Flow ()
updateOrganizationIdAndMakeAdmin personId orgId = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause orgId now) (predicate personId)
    >>= either throwDBError pure
  where
    setClause sOrgId n Storage.Person {..} =
      mconcat
        [ _organizationId <-. B.val_ (Just sOrgId),
          _role <-. B.val_ Storage.ADMIN,
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

updatePersonRec :: Id Storage.Person -> Storage.Person -> Flow ()
updatePersonRec personId uperson = do
  dbTable <- getDbTable
  now <- getCurrTime
  person <- encrypt uperson
  DB.update dbTable (setClause person now) (predicate personId)
    >>= either throwDBError pure
  where
    setClause person n Storage.Person {..} =
      mconcat
        [ _firstName <-. B.val_ (Storage._firstName person),
          _middleName <-. B.val_ (Storage._middleName person),
          _lastName <-. B.val_ (Storage._lastName person),
          _fullName <-. B.val_ (Storage._fullName person),
          _role <-. B.val_ (Storage._role person),
          _gender <-. B.val_ (Storage._gender person),
          _email <-. B.val_ (Storage._email person),
          _identifier <-. B.val_ (Storage._identifier person),
          _rating <-. B.val_ (Storage._rating person),
          _deviceToken <-. B.val_ (Storage._deviceToken person),
          _udf1 <-. B.val_ (Storage._udf1 person),
          _udf2 <-. B.val_ (Storage._udf2 person),
          _organizationId <-. B.val_ (Storage._organizationId person),
          _description <-. B.val_ (Storage._description person),
          _locationId <-. B.val_ (Storage._locationId person),
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

updatePerson :: Id Storage.Person -> Bool -> Text -> Storage.IdentifierType -> Maybe Text -> Flow ()
updatePerson personId verified identifier identifierType mobileNumber = do
  dbTable <- getDbTable
  now <- getCurrTime
  mobileNumber' <- encrypt mobileNumber
  DB.update
    dbTable
    (setClause identifier identifierType mobileNumber' verified now)
    (predicate personId)
    >>= either throwDBError pure
  where
    setClause i it mn v n Storage.Person {..} =
      mconcat
        [ _identifier <-. B.val_ (Just i),
          _identifierType <-. B.val_ it,
          _mobileNumber <-. B.val_ mn,
          _verified <-. B.val_ v,
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

update ::
  Id Storage.Person ->
  Storage.Status ->
  Bool ->
  Maybe FCM.FCMRecipientToken ->
  Flow ()
update id status verified deviceTokenM = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status verified currTime deviceTokenM)
    (predicate id)
    >>= either throwDBError pure
  where
    setClause sStatus sVerified currTime deviceToken Storage.Person {..} =
      mconcat
        [ _status <-. B.val_ sStatus,
          _updatedAt <-. B.val_ currTime,
          _verified <-. B.val_ sVerified,
          _deviceToken <-. B.val_ deviceToken
        ]
    predicate pid Storage.Person {..} = _id ==. B.val_ pid

deleteById :: Id Storage.Person -> Flow ()
deleteById id = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate id)
    >>= either throwDBError pure
  where
    predicate pid Storage.Person {..} = _id ==. B.val_ pid

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
    >>= either throwDBError pure
  where
    setClause mEntityId mEntityType Storage.Person {..} =
      mconcat
        [ _udf1 <-. B.val_ mEntityId,
          _udf2 <-. B.val_ mEntityType
        ]
    predicate pId Storage.Person {..} = _id ==. B.val_ pId

findByEntityId :: Text -> Flow (Maybe Storage.Person)
findByEntityId entityId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _udf1 ==. B.val_ (Just entityId)

updateAverageRating :: Id Storage.Person -> Text -> Flow ()
updateAverageRating personId newAverageRating = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause newAverageRating now) (predicate personId)
    >>= either throwDBError pure
  where
    setClause rating now Storage.Person {..} =
      mconcat
        [ _rating <-. B.val_ (Just rating),
          _updatedAt <-. B.val_ now
        ]
    predicate pId Storage.Person {..} = _id ==. B.val_ pId

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
    >>= either throwDBError pure
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
            (driver ^. #_locationId)
      dist <- distToPoint point location
      driver' <- B.filter_ (predicate orgId radius dist) (pure driver)
      return (driver', dist)
    orderBy (_, dist) = B.asc_ dist
    predicate orgId radius dist Storage.Person {..} =
      _role ==. B.val_ Storage.DRIVER
        &&. _organizationId ==. B.val_ orgId
        &&. dist <. B.val_ radius

    distToPoint' :: (Monoid a, IsString a) => a -> a -> a -> a
    distToPoint' lat lon point =
      point <> " <-> ST_Point(" <> lat <> ", " <> lon <> ")::geometry"
    distToPoint LatLong {..} location =
      B.customExpr_ distToPoint' lat lon (location ^. #_point)
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
