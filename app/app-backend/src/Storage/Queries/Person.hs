{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Person where

import App.Types
import Beckn.External.Encryption
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.ID
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.Person as Storage
import Beckn.Utils.Common (getCurrTime, getSchemaName)
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.PersonT))
getDbTable =
  DB._person . DB.appDb <$> getSchemaName

create :: Storage.Person -> Flow ()
create person = do
  dbTable <- getDbTable
  person' <- encrypt person
  DB.createOne dbTable (Storage.insertExpression person')
    >>= either DB.throwDBError pure

findById ::
  ID Storage.Person -> Flow (Maybe Storage.Person)
findById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} = _id ==. B.val_ id

findAllByOrgIds ::
  [Storage.Role] -> [Text] -> Flow [Storage.Person]
findAllByOrgIds roles orgIds = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable (predicate roles orgIds)
    >>= decrypt
  where
    predicate pRoles pOrgIds Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> pRoles) ||. complementVal roles,
          _organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal pOrgIds
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findByIdentifier ::
  Storage.IdentifierType -> Text -> Flow (Maybe Storage.Person)
findByIdentifier idType mb = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _identifierType ==. B.val_ idType
        &&. (_mobileNumber ^. #_hash) ==. B.val_ (Just $ evalDbHash mb)

findByUsernameAndPassword ::
  Text -> Text -> Flow (Maybe Storage.Person)
findByUsernameAndPassword email password = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _email ==. B.val_ (Just email)
        &&. _passwordHash ==. B.val_ (Just $ evalDbHash password)

findByRoleAndMobileNumber ::
  Storage.Role -> Text -> Text -> Flow (Maybe Storage.Person)
findByRoleAndMobileNumber role countryCode mobileNumber = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _role ==. B.val_ role
        &&. _mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (_mobileNumber ^. #_hash) ==. B.val_ (Just $ evalDbHash mobileNumber)

findByRoleAndMobileNumberWithoutCC :: Storage.Role -> Text -> Flow (Maybe Storage.Person)
findByRoleAndMobileNumberWithoutCC role mobileNumber = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    predicate Storage.Person {..} =
      _role ==. B.val_ role
        &&. (_mobileNumber ^. #_hash) ==. B.val_ (Just $ evalDbHash mobileNumber)

updateMultiple :: ID Storage.Person -> Storage.Person -> Flow ()
updateMultiple personId person = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause now person) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause now (sPerson :: Storage.Person) Storage.Person {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _firstName <-. B.val_ (sPerson ^. #_firstName),
          _middleName <-. B.val_ (sPerson ^. #_middleName),
          _lastName <-. B.val_ (sPerson ^. #_lastName),
          _fullName <-. B.val_ (sPerson ^. #_fullName),
          _gender <-. B.val_ (sPerson ^. #_gender),
          _email <-. B.val_ (sPerson ^. #_email),
          _organizationId <-. B.val_ (sPerson ^. #_organizationId),
          _locationId <-. B.val_ (sPerson ^. #_locationId),
          _description <-. B.val_ (sPerson ^. #_description),
          _status <-. B.val_ (sPerson ^. #_status),
          _role <-. B.val_ (sPerson ^. #_role),
          _identifier <-. B.val_ (sPerson ^. #_identifier),
          _rating <-. B.val_ (sPerson ^. #_rating),
          _deviceToken <-. B.val_ (sPerson ^. #_deviceToken),
          _udf1 <-. B.val_ (sPerson ^. #_udf1),
          _udf2 <-. B.val_ (sPerson ^. #_udf2)
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

update ::
  ID Storage.Person ->
  Maybe Storage.Status ->
  Maybe Text ->
  Maybe Text ->
  Maybe Storage.Role ->
  Maybe Storage.IdentifierType ->
  Maybe Text ->
  Flow ()
update id statusM nameM emailM roleM identTypeM identM = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause statusM nameM emailM roleM identM identTypeM currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause scStatusM scNameM scEmailM scRoleM scIdentM scIdentTypeM currTime Storage.Person {..} =
      mconcat
        ( [ _updatedAt <-. B.val_ currTime
          ]
            <> (\name -> [_fullName <-. B.val_ name]) scNameM
            <> (\email -> [_email <-. B.val_ email]) scEmailM
            <> maybe [] (\role -> [_role <-. B.val_ role]) scRoleM
            <> maybe [] (\status -> [_status <-. B.val_ status]) scStatusM
            <> maybe [] (\iden -> [_identifier <-. B.val_ (Just iden)]) scIdentM
            <> maybe [] (\idT -> [_identifierType <-. B.val_ idT]) scIdentTypeM
        )
    predicate pid Storage.Person {..} = _id ==. B.val_ pid

updatePersonOrgId :: Text -> ID Storage.Person -> Flow ()
updatePersonOrgId orgId personId = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause orgId now) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause a n Storage.Person {..} =
      mconcat [_organizationId <-. B.val_ (Just a), _updatedAt <-. B.val_ n]
    predicate i Storage.Person {..} = _id ==. B.val_ i

updatePersonalInfo :: ID Storage.Person -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Storage.Gender -> Maybe Text -> Maybe FCM.FCMRecipientToken -> Flow ()
updatePersonalInfo personId mbFirstName mbMiddleName mbLastName mbFullName mbGender mbEmail mbDeviceToken = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause now mbFirstName mbMiddleName mbLastName mbFullName mbGender mbEmail mbDeviceToken) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause now mbFirstN mbMiddleN mbLastN mbFullN mbG mbE mbDToken Storage.Person {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          maybe mempty (\x -> _firstName <-. B.val_ (Just x)) mbFirstN,
          maybe mempty (\x -> _middleName <-. B.val_ (Just x)) mbMiddleN,
          maybe mempty (\x -> _lastName <-. B.val_ (Just x)) mbLastN,
          maybe mempty (\x -> _fullName <-. B.val_ (Just x)) mbFullN,
          maybe mempty (\x -> _gender <-. B.val_ x) mbG,
          maybe mempty (\x -> _email <-. B.val_ (Just x)) mbE,
          maybe mempty (\x -> _deviceToken <-. B.val_ (Just x)) mbDToken
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

findAllWithLimitOffsetByRole :: Maybe Int -> Maybe Int -> [Storage.Role] -> Flow [Storage.Person]
findAllWithLimitOffsetByRole mlimit moffset roles = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable (predicate roles) limit offset orderByDesc
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate [] Storage.Person {..} = B.val_ True
    predicate r Storage.Person {..} =
      _role `B.in_` (B.val_ <$> r)
    orderByDesc Storage.Person {..} = B.desc_ _createdAt

findAllWithLimitOffsetBy :: Maybe Int -> Maybe Int -> [Storage.Role] -> [ID Organization] -> Flow [Storage.Person]
findAllWithLimitOffsetBy mlimit moffset roles orgIds = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable (predicate orgIds roles) limit offset orderByDesc
    >>= either DB.throwDBError pure
    >>= decrypt
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate pOrgIds [] Storage.Person {..} =
      _organizationId `B.in_` (B.val_ . Just . getId <$> pOrgIds)
    predicate pOrgIds pRoles Storage.Person {..} =
      _organizationId `B.in_` (B.val_ . Just . getId <$> pOrgIds) &&. _role `B.in_` (B.val_ <$> pRoles)
    orderByDesc Storage.Person {..} = B.desc_ _createdAt

deleteById :: ID Storage.Person -> Flow ()
deleteById id = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate pid Storage.Person {..} = _id ==. B.val_ pid
