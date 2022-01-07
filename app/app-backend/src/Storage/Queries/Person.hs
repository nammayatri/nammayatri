module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.External.FCM.Types (FCMRecipientToken)
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import Storage.Tabular.Person

create :: Person -> SqlDB ()
create = create'

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Person)
findById = Esq.findById

findByEmailAndPassword ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByEmailAndPassword email_ password = do
  passwordDbHash <- getDbHash password
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmail ==. val (Just email_)
        &&. person ^. PersonPasswordHash ==. val (Just passwordDbHash)
    return person

findByRoleAndMobileNumber ::
  (Transactionable m, EncFlow m r) =>
  Role ->
  Text ->
  Text ->
  m (Maybe Person)
findByRoleAndMobileNumber role_ countryCode mobileNumber_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
    return person

findByRoleAndMobileNumberWithoutCC :: (Transactionable m, EncFlow m r) => Role -> Text -> m (Maybe Person)
findByRoleAndMobileNumberWithoutCC role_ mobileNumber_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
    return person

updateMultiple :: Id Person -> Person -> SqlDB ()
updateMultiple personId person = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ PersonUpdatedAt =. val now,
        PersonFirstName =. val (person.firstName),
        PersonMiddleName =. val (person.middleName),
        PersonLastName =. val (person.lastName),
        PersonGender =. val (person.gender),
        PersonEmail =. val (person.email),
        PersonDescription =. val (person.description),
        PersonRole =. val (person.role),
        PersonIdentifier =. val (person.identifier),
        PersonRating =. val (person.rating),
        PersonDeviceToken =. val (person.deviceToken),
        PersonUdf1 =. val (person.udf1),
        PersonUdf2 =. val (person.udf2)
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

updateDeviceToken :: Id Person -> Maybe FCMRecipientToken -> SqlDB ()
updateDeviceToken personId mbDeviceToken = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ PersonUpdatedAt =. val now,
        PersonDeviceToken =. val mbDeviceToken
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

setIsNewFalse :: Id Person -> SqlDB ()
setIsNewFalse personId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ PersonUpdatedAt =. val now,
        PersonIsNew =. val False
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

updatePersonalInfo ::
  Id Person ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe FCMRecipientToken ->
  SqlDB ()
updatePersonalInfo personId mbFirstName mbMiddleName mbLastName mbDeviceToken = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      ( [PersonUpdatedAt =. val now]
          <> updateWhenJust_ (\x -> PersonFirstName =. val (Just x)) mbFirstName
          <> updateWhenJust_ (\x -> PersonMiddleName =. val (Just x)) mbMiddleName
          <> updateWhenJust_ (\x -> PersonLastName =. val (Just x)) mbLastName
          <> updateWhenJust_ (\x -> PersonDeviceToken =. val (Just x)) mbDeviceToken
      )
    where_ $ tbl ^. PersonId ==. val (getId personId)
