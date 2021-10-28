module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Person as Storage

getDbTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.PersonT))
getDbTable =
  DB.person . DB.appDb <$> getSchemaName

create :: DBFlow m r => Storage.Person -> m ()
create person = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue person)

findById ::
  DBFlow m r =>
  Id Storage.Person ->
  m (Maybe Storage.Person)
findById personId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Person {..} = id ==. B.val_ personId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findByEmailAndPassword ::
  (DBFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Storage.Person)
findByEmailAndPassword email_ password = do
  dbTable <- getDbTable
  passwordDbHash <- getDbHash password
  DB.findOne dbTable (predicate passwordDbHash)
  where
    predicate passwordDbHash Storage.Person {..} =
      email ==. B.val_ (Just email_)
        &&. passwordHash ==. B.val_ (Just passwordDbHash)

findByRoleAndMobileNumber ::
  (DBFlow m r, EncFlow m r) =>
  Storage.Role ->
  Text ->
  Text ->
  m (Maybe Storage.Person)
findByRoleAndMobileNumber role_ countryCode mobileNumber_ = do
  dbTable <- getDbTable
  mobileNumberDbHash <- getDbHash mobileNumber_
  DB.findOne dbTable (predicate mobileNumberDbHash)
  where
    predicate mobileNumberDbHash Storage.Person {..} =
      role ==. B.val_ role_
        &&. mobileCountryCode ==. B.val_ (Just countryCode)
        &&. (mobileNumber.hash) ==. B.val_ (Just mobileNumberDbHash)

findByRoleAndMobileNumberWithoutCC :: (DBFlow m r, EncFlow m r) => Storage.Role -> Text -> m (Maybe Storage.Person)
findByRoleAndMobileNumberWithoutCC role_ mobileNumber_ = do
  dbTable <- getDbTable
  mobileNumberDbHash <- getDbHash mobileNumber_
  DB.findOne dbTable (predicate mobileNumberDbHash)
  where
    predicate mobileNumberDbHash Storage.Person {..} =
      role ==. B.val_ role_
        &&. (mobileNumber.hash) ==. B.val_ (Just mobileNumberDbHash)

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
          description <-. B.val_ (sPerson.description),
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
  Maybe Text ->
  Maybe Text ->
  Maybe Storage.Role ->
  Maybe Storage.IdentifierType ->
  Maybe Text ->
  m ()
update personId nameM emailM roleM identTypeM identM = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause nameM emailM roleM identM identTypeM currTime)
    (predicate personId)
  where
    setClause scNameM scEmailM scRoleM scIdentM scIdentTypeM currTime Storage.Person {..} =
      mconcat
        ( [ updatedAt <-. B.val_ currTime
          ]
            <> (\name -> [fullName <-. B.val_ name]) scNameM
            <> (\email_ -> [email <-. B.val_ email_]) scEmailM
            <> maybe [] (\role_ -> [role <-. B.val_ role_]) scRoleM
            <> maybe [] (\iden -> [identifier <-. B.val_ (Just iden)]) scIdentM
            <> maybe [] (\idT -> [identifierType <-. B.val_ idT]) scIdentTypeM
        )
    predicate pid Storage.Person {..} = id ==. B.val_ pid

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

updatePersonalInfo ::
  DBFlow m r =>
  Id Storage.Person ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe FCM.FCMRecipientToken ->
  m ()
updatePersonalInfo personId mbFirstName mbMiddleName mbLastName mbDeviceToken = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause now mbFirstName mbMiddleName mbLastName mbDeviceToken) (predicate personId)
  where
    setClause now mbFirstN mbMiddleN mbLastN mbDToken Storage.Person {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          maybe mempty (\x -> firstName <-. B.val_ (Just x)) mbFirstN,
          maybe mempty (\x -> middleName <-. B.val_ (Just x)) mbMiddleN,
          maybe mempty (\x -> lastName <-. B.val_ (Just x)) mbLastN,
          maybe mempty (\x -> deviceToken <-. B.val_ (Just x)) mbDToken
        ]
    predicate personId_ Storage.Person {..} = id ==. B.val_ personId_

deleteById :: DBFlow m r => Id Storage.Person -> m ()
deleteById pid = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate pid)
  where
    predicate pid_ Storage.Person {..} = id ==. B.val_ pid_
