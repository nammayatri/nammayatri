{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Person where

import App.Types
import Beckn.External.Encryption
import Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as Storage
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.PersonT)
dbTable = DB._person DB.transporterDb

create :: Storage.Person -> Flow ()
create person = do
  person' <- encrypt person
  DB.createOne dbTable (Storage.insertExpression person')
    >>= either DB.throwDBError pure

findPersonById ::
  PersonId -> Flow Storage.Person
findPersonById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "PERSON_NOT_FOUND"
    >>= decrypt
  where
    predicate Storage.Person {..} = _id ==. B.val_ id

findPersonByIdAndRoleAndOrgId :: PersonId -> Storage.Role -> Text -> Flow (Maybe Storage.Person)
findPersonByIdAndRoleAndOrgId id role orgId =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _id ==. B.val_ id
        &&. _role ==. B.val_ role
        &&. _organizationId ==. B.val_ (Just orgId)

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Storage.Role] -> [Text] -> Flow [Storage.Person]
findAllWithLimitOffsetByOrgIds mlimit moffset roles orgIds =
  DB.findAllWithLimitOffsetWhere dbTable (predicate roles orgIds) limit offset orderByDesc
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    orderByDesc Storage.Person {..} = B.desc_ _createdAt
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate roles orgIds Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          _organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal orgIds
        ]

findAllByOrgIds ::
  [Storage.Role] -> [Text] -> Flow [Storage.Person]
findAllByOrgIds roles orgIds =
  DB.findAllOrErr dbTable (predicate roles orgIds)
    >>= decrypt
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

findByMobileNumber ::
  Text -> Text -> Flow (Maybe Storage.Person)
findByMobileNumber countryCode mobileNumber =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (_mobileNumber ^. #_hash) ==. B.val_ (Just $ evalDbHash mobileNumber)

findByIdentifier ::
  Text -> Flow (Maybe Storage.Person)
findByIdentifier identifier =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _identifier ==. B.val_ (Just identifier)

findByEmail ::
  Text -> Flow (Maybe Storage.Person)
findByEmail email =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _email ==. B.val_ (Just email)

updateOrganizationIdAndMakeAdmin :: PersonId -> Text -> Flow ()
updateOrganizationIdAndMakeAdmin personId orgId = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause orgId now) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause orgId n Storage.Person {..} =
      mconcat
        [ _organizationId <-. B.val_ (Just orgId),
          _role <-. B.val_ Storage.ADMIN,
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

updatePersonRec :: PersonId -> Storage.Person -> Flow ()
updatePersonRec personId uperson = do
  now <- getCurrentTimeUTC
  person <- encrypt uperson
  DB.update dbTable (setClause person now) (predicate personId)
    >>= either DB.throwDBError pure
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

updatePerson :: PersonId -> Bool -> Text -> Storage.IdentifierType -> Maybe Text -> Flow ()
updatePerson personId verified identifier identifierType mobileNumber = do
  now <- getCurrentTimeUTC
  mobileNumber' <- encrypt mobileNumber
  DB.update
    dbTable
    (setClause identifier identifierType mobileNumber' verified now)
    (predicate personId)
    >>= either DB.throwDBError pure
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
  PersonId ->
  Storage.Status ->
  Bool ->
  Maybe FCM.FCMRecipientToken ->
  Flow ()
update id status verified deviceTokenM = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status verified currTime deviceTokenM)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status verified currTime deviceToken Storage.Person {..} =
      mconcat
        [ _status <-. B.val_ status,
          _updatedAt <-. B.val_ currTime,
          _verified <-. B.val_ verified,
          _deviceToken <-. B.val_ deviceToken
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

deleteById :: PersonId -> Flow ()
deleteById id =
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Person {..} = _id ==. B.val_ id

updateEntity :: PersonId -> Text -> Text -> Flow ()
updateEntity personId entityId entityType = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause entityId entityType)
    (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause entityId entityType Storage.Person {..} =
      mconcat
        [ _udf1 <-. B.val_ (Just entityId),
          _udf2 <-. B.val_ (Just entityType)
        ]
    predicate personId Storage.Person {..} = _id ==. B.val_ personId
