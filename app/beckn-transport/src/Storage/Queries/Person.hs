module Storage.Queries.Person where

import Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Person as Storage
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.PersonT)
dbTable = DB._person DB.transporterDb

create :: Storage.Person -> L.Flow ()
create Storage.Person {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Person {..})
    >>= either DB.throwDBError pure

findPersonById ::
  PersonId -> L.Flow Storage.Person
findPersonById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_DATA"
  where
    predicate Storage.Person {..} = (_id ==. B.val_ id)

findPersonByIdAndRoleAndOrgId :: PersonId -> Storage.Role -> Text -> L.Flow (Maybe Storage.Person)
findPersonByIdAndRoleAndOrgId id role orgId = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} =
      ( _id ==. B.val_ id
          &&. _role ==. B.val_ role
          &&. _organizationId ==. B.val_ (Just orgId)
      )

findAllWithLimitOffsetByOrgIds :: Maybe Integer -> Maybe Integer -> [Storage.Role] -> [Text] -> L.Flow [Storage.Person]
findAllWithLimitOffsetByOrgIds mlimit moffset roles orgIds = do
  DB.findAllWithLimitOffsetWhere dbTable (predicate roles orgIds) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Person {..} = B.desc_ _createdAt
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    predicate roles orgIds Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          _organizationId `B.in_` ((\x -> B.val_ $ Just x) <$> orgIds) ||. complementVal orgIds
        ]

findAllByOrgIds ::
  [Storage.Role] -> [Text] -> L.Flow [Storage.Person]
findAllByOrgIds roles orgIds = do
  DB.findAllOrErr dbTable (predicate roles orgIds)
  where
    predicate roles orgIds Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> roles) ||. complementVal roles,
          _organizationId `B.in_` ((\x -> B.val_ $ Just x) <$> orgIds) ||. complementVal orgIds
        ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False

findByMobileNumber ::
  Text -> L.Flow (Maybe Storage.Person)
findByMobileNumber mobileNumber =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} =
      _mobileNumber ==. B.val_ (Just mobileNumber)

updateOrganizationIdAndMakeAdmin :: PersonId -> Text -> L.Flow ()
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

updatePersonRec :: PersonId -> Storage.Person -> L.Flow ()
updatePersonRec personId person = do
  now <- getCurrentTimeUTC
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

updatePerson :: PersonId -> Bool -> Text -> Storage.IdentifierType -> Maybe Text -> L.Flow ()
updatePerson personId verified identifier identifierType mobileNumber = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause identifier identifierType mobileNumber verified now) (predicate personId)
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
  L.Flow ()
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
        ( [ _status <-. B.val_ status,
            _updatedAt <-. B.val_ currTime,
            _verified <-. B.val_ verified,
            _deviceToken <-. B.val_ deviceToken
          ]
        )
    predicate id Storage.Person {..} = _id ==. B.val_ id

findByAnyOf :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> L.Flow (Maybe Storage.Person)
findByAnyOf idM mobileM emailM identifierM =
  DB.findOne dbTable (predicate idM mobileM emailM identifierM)
    >>= either DB.throwDBError pure
  where
    predicate idM mobileM emailM identifierM Storage.Person {..} =
      ( (B.val_ (isNothing idM) ||. _id ==. B.val_ (PersonId (fromMaybe "DONT_MATCH" idM)))
          &&. (B.val_ (isNothing identifierM) ||. _identifier ==. B.val_ identifierM)
          &&. (B.val_ (isNothing mobileM) ||. _mobileNumber ==. B.val_ mobileM)
          &&. (B.val_ (isNothing emailM) ||. _email ==. B.val_ emailM)
      )

deleteById :: PersonId -> L.Flow ()
deleteById id = do
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Person {..} = (_id ==. B.val_ id)
