module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Storage

getDbTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.PersonT))
getDbTable =
  DB.person . DB.appDb <$> getSchemaName

create :: DBFlow m r => Storage.Person -> m ()
create person = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression person)

findById ::
  DBFlow m r =>
  Id Storage.Person ->
  m (Maybe Storage.Person)
findById personId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} = id ==. B.val_ personId

findAllByOrgIds ::
  DBFlow m r =>
  [Storage.Role] ->
  [Id Org.Organization] ->
  m [Storage.Person]
findAllByOrgIds roles orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate roles orgIds)
  where
    predicate pRoles pOrgIds Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ role `B.in_` (B.val_ <$> pRoles) ||. complementVal roles,
          organizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal pOrgIds
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findByUsernameAndPassword ::
  DBFlow m r =>
  Text ->
  Text ->
  m (Maybe Storage.Person)
findByUsernameAndPassword email_ password = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      email ==. B.val_ (Just email_)
        &&. passwordHash ==. B.val_ (Just $ evalDbHash password)

findByRoleAndMobileNumber ::
  DBFlow m r =>
  Storage.Role ->
  Text ->
  Text ->
  m (Maybe Storage.Person)
findByRoleAndMobileNumber role_ countryCode mobileNumber_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      role ==. B.val_ role_
        &&. mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (mobileNumber.hash) ==. B.val_ (Just $ evalDbHash mobileNumber_)

findByRoleAndMobileNumberWithoutCC :: DBFlow m r => Storage.Role -> Text -> m (Maybe Storage.Person)
findByRoleAndMobileNumberWithoutCC role_ mobileNumber_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} =
      role ==. B.val_ role_
        &&. (mobileNumber.hash) ==. B.val_ (Just $ evalDbHash mobileNumber_)

updateMultiple :: Id Storage.Person -> Storage.Person -> DB.SqlDB ()
updateMultiple personId person = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause now person) (predicate personId)
  where
    setClause now (sPerson :: Storage.Person) Storage.Person {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          firstName <-. B.val_ (sPerson.firstName),
          middleName <-. B.val_ (sPerson.middleName),
          lastName <-. B.val_ (sPerson.lastName),
          fullName <-. B.val_ (sPerson.fullName),
          gender <-. B.val_ (sPerson.gender),
          email <-. B.val_ (sPerson.email),
          organizationId <-. B.val_ (sPerson.organizationId),
          locationId <-. B.val_ (sPerson.locationId),
          description <-. B.val_ (sPerson.description),
          status <-. B.val_ (sPerson.status),
          role <-. B.val_ (sPerson.role),
          identifier <-. B.val_ (sPerson.identifier),
          rating <-. B.val_ (sPerson.rating),
          deviceToken <-. B.val_ (sPerson.deviceToken),
          udf1 <-. B.val_ (sPerson.udf1),
          udf2 <-. B.val_ (sPerson.udf2)
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

updateDeviceToken :: Id Storage.Person -> Maybe FCMRecipientToken -> DB.SqlDB ()
updateDeviceToken personId mbDeviceToken = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause now mbDeviceToken) (predicate personId)
  where
    setClause now mbDeviceToken_ Storage.Person {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          deviceToken <-. B.val_ mbDeviceToken_
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

update ::
  DBFlow m r =>
  Id Storage.Person ->
  Maybe Storage.Status ->
  Maybe Text ->
  Maybe Text ->
  Maybe Storage.Role ->
  Maybe Storage.IdentifierType ->
  Maybe Text ->
  m ()
update personId statusM nameM emailM roleM identTypeM identM = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause statusM nameM emailM roleM identM identTypeM currTime)
    (predicate personId)
  where
    setClause scStatusM scNameM scEmailM scRoleM scIdentM scIdentTypeM currTime Storage.Person {..} =
      mconcat
        ( [ updatedAt <-. B.val_ currTime
          ]
            <> (\name -> [fullName <-. B.val_ name]) scNameM
            <> (\email_ -> [email <-. B.val_ email_]) scEmailM
            <> maybe [] (\role_ -> [role <-. B.val_ role_]) scRoleM
            <> maybe [] (\status_ -> [status <-. B.val_ status_]) scStatusM
            <> maybe [] (\iden -> [identifier <-. B.val_ (Just iden)]) scIdentM
            <> maybe [] (\idT -> [identifierType <-. B.val_ idT]) scIdentTypeM
        )
    predicate pid Storage.Person {..} = id ==. B.val_ pid

updatePersonalInfo ::
  DBFlow m r =>
  Id Storage.Person ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Storage.Gender ->
  Maybe Text ->
  Maybe FCM.FCMRecipientToken ->
  m ()
updatePersonalInfo personId mbFirstName mbMiddleName mbLastName mbFullName mbGender mbEmail mbDeviceToken = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause now mbFirstName mbMiddleName mbLastName mbFullName mbGender mbEmail mbDeviceToken) (predicate personId)
  where
    setClause now mbFirstN mbMiddleN mbLastN mbFullN mbG mbE mbDToken Storage.Person {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          maybe mempty (\x -> firstName <-. B.val_ (Just x)) mbFirstN,
          maybe mempty (\x -> middleName <-. B.val_ (Just x)) mbMiddleN,
          maybe mempty (\x -> lastName <-. B.val_ (Just x)) mbLastN,
          maybe mempty (\x -> fullName <-. B.val_ (Just x)) mbFullN,
          maybe mempty (\x -> gender <-. B.val_ x) mbG,
          maybe mempty (\x -> email <-. B.val_ (Just x)) mbE,
          maybe mempty (\x -> deviceToken <-. B.val_ (Just x)) mbDToken
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

findAllWithLimitOffsetBy :: DBFlow m r => Maybe Int -> Maybe Int -> [Storage.Role] -> [Id Organization] -> m [Storage.Person]
findAllWithLimitOffsetBy mlimit moffset roles orgIds = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) (predicate orgIds roles)
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate pOrgIds [] Storage.Person {..} =
      organizationId `B.in_` (B.val_ . Just <$> pOrgIds)
    predicate pOrgIds pRoles Storage.Person {..} =
      organizationId `B.in_` (B.val_ . Just <$> pOrgIds) &&. role `B.in_` (B.val_ <$> pRoles)
    orderByDesc Storage.Person {..} = B.desc_ createdAt

deleteById :: DBFlow m r => Id Storage.Person -> m ()
deleteById pid = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate pid)
  where
    predicate pid_ Storage.Person {..} = id ==. B.val_ pid_
