{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Person where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as Storage
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import Epass.Types.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.PersonT)
dbTable = DB._person DB.appDb

create :: Storage.Person -> L.Flow ()
create Storage.Person {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Person {..})
    >>= either DB.throwDBError pure

findById ::
  PersonId -> L.Flow (Maybe Storage.Person)
findById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} = _id ==. B.val_ id

findAllByOrgIds ::
  [Storage.Role] -> [Text] -> L.Flow [Storage.Person]
findAllByOrgIds roles orgIds =
  DB.findAllOrErr dbTable (predicate roles orgIds)
  where
    predicate roles orgIds Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          _organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal orgIds
        ]

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findByIdentifier ::
  Storage.IdentifierType -> Text -> L.Flow (Maybe Storage.Person)
findByIdentifier idType mb =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} =
      _identifierType ==. B.val_ idType
        &&. _mobileNumber ==. B.val_ (Just mb)

findByRoleAndIdentifier ::
  Storage.Role -> Storage.IdentifierType -> Text -> L.Flow (Maybe Storage.Person)
findByRoleAndIdentifier role idType identifier =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} =
      _role ==. B.val_ role
        &&. _mobileNumber ==. B.val_ (Just identifier)

updateMultiple :: PersonId -> Storage.Person -> L.Flow ()
updateMultiple personId person = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause now person) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause now person Storage.Person {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _firstName <-. B.val_ (person ^. #_firstName),
          _middleName <-. B.val_ (person ^. #_middleName),
          _lastName <-. B.val_ (person ^. #_lastName),
          _fullName <-. B.val_ (person ^. #_fullName),
          _gender <-. B.val_ (person ^. #_gender),
          _email <-. B.val_ (person ^. #_email),
          _organizationId <-. B.val_ (person ^. #_organizationId),
          _locationId <-. B.val_ (person ^. #_locationId),
          _description <-. B.val_ (person ^. #_description),
          _status <-. B.val_ (person ^. #_status),
          _role <-. B.val_ (person ^. #_role),
          _identifier <-. B.val_ (person ^. #_identifier),
          _rating <-. B.val_ (person ^. #_rating),
          _deviceToken <-. B.val_ (person ^. #_deviceToken),
          _udf1 <-. B.val_ (person ^. #_udf1),
          _udf2 <-. B.val_ (person ^. #_udf2)
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

update ::
  PersonId ->
  Maybe Storage.Status ->
  Maybe Text ->
  Maybe Text ->
  Maybe Storage.Role ->
  Maybe Storage.IdentifierType ->
  Maybe Text ->
  L.Flow ()
update id statusM nameM emailM roleM identTypeM identM = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause statusM nameM emailM roleM identM identTypeM currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause statusM nameM emailM roleM identM identTypeM currTime Storage.Person {..} =
      mconcat
        ( [ _updatedAt <-. B.val_ currTime
          ]
            <> (\name -> [_fullName <-. B.val_ name]) nameM
            <> (\email -> [_email <-. B.val_ email]) emailM
            <> maybe [] (\role -> [_role <-. B.val_ role]) roleM
            <> maybe [] (\status -> [_status <-. B.val_ status]) statusM
            <> maybe [] (\iden -> [_identifier <-. B.val_ (Just iden)]) identM
            <> maybe [] (\idT -> [_identifierType <-. B.val_ idT]) identTypeM
        )
    predicate id Storage.Person {..} = _id ==. B.val_ id

updatePersonOrgId :: Text -> PersonId -> L.Flow ()
updatePersonOrgId orgId personId = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause orgId now) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause a n Storage.Person {..} =
      mconcat [_organizationId <-. B.val_ (Just a), _updatedAt <-. B.val_ n]
    predicate i Storage.Person {..} = _id ==. B.val_ i

findAllWithLimitOffsetByRole :: Maybe Int -> Maybe Int -> [Storage.Role] -> L.Flow [Storage.Person]
findAllWithLimitOffsetByRole mlimit moffset roles =
  DB.findAllWithLimitOffsetWhere dbTable (predicate roles) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate [] Storage.Person {..} = B.val_ True
    predicate r Storage.Person {..} =
      _role `B.in_` (B.val_ <$> r)
    orderByDesc Storage.Person {..} = B.desc_ _createdAt

findAllWithLimitOffsetBy :: Maybe Int -> Maybe Int -> [Storage.Role] -> [OrganizationId] -> L.Flow [Storage.Person]
findAllWithLimitOffsetBy mlimit moffset roles orgIds =
  DB.findAllWithLimitOffsetWhere dbTable (predicate orgIds roles) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate orgIds [] Storage.Person {..} =
      _organizationId `B.in_` (B.val_ . Just . _getOrganizationId <$> orgIds)
    predicate orgIds roles Storage.Person {..} =
      _organizationId `B.in_` (B.val_ . Just . _getOrganizationId <$> orgIds) &&. _role `B.in_` (B.val_ <$> roles)
    orderByDesc Storage.Person {..} = B.desc_ _createdAt

deleteById :: PersonId -> L.Flow ()
deleteById id =
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Person {..} = _id ==. B.val_ id
