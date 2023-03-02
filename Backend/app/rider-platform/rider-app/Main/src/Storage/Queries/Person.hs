{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person where

import Control.Applicative ((<|>))
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person
import Kernel.External.Encryption
import Kernel.External.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import Storage.Tabular.Person

create :: Person -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Proxy ma ->
  m (Maybe Person)
findById personId _ = Esq.findById @m @ma personId

findByEmailAndPassword ::
  forall m ma r.
  (Transactionable ma m, EncFlow m r) =>
  Text ->
  Text ->
  Proxy ma ->
  m (Maybe Person)
findByEmailAndPassword email_ password _ = do
  emailDbHash <- getDbHash email_
  passwordDbHash <- getDbHash password
  findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val (Just emailDbHash)
        &&. person ^. PersonPasswordHash ==. val (Just passwordDbHash)
    return person

findByEmail ::
  forall m ma r.
  (Transactionable ma m, EncFlow m r) =>
  Text ->
  Proxy ma ->
  m (Maybe Person)
findByEmail email_ _ = do
  emailDbHash <- getDbHash email_
  findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val (Just emailDbHash)
    return person

findByRoleAndMobileNumberAndMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Role ->
  Text ->
  DbHash ->
  Id Merchant ->
  Proxy ma ->
  m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumberHash merchantId _ = do
  findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByRoleAndMobileNumberAndMerchantIdWithoutCC :: forall m ma. Transactionable ma m => Role -> DbHash -> Id Merchant -> Proxy ma -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantIdWithoutCC role_ mobileNumberHash merchantId _ = do
  findOne @m @ma $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

updateMultiple :: Id Person -> Person -> SqlDB m ()
updateMultiple personId person = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonUpdatedAt =. val now,
        PersonFirstName =. val (person.firstName),
        PersonMiddleName =. val (person.middleName),
        PersonLastName =. val (person.lastName),
        PersonGender =. val (person.gender),
        PersonDescription =. val (person.description),
        PersonRole =. val (person.role),
        PersonIdentifier =. val (person.identifier),
        PersonRating =. val (person.rating),
        PersonDeviceToken =. val (person.deviceToken),
        PersonClientVersion =. val (versionToText <$> person.clientVersion),
        PersonBundleVersion =. val (versionToText <$> person.bundleVersion)
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB m ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      Esq.update $ \tbl -> do
        set
          tbl
          [ PersonUpdatedAt =. val now,
            PersonClientVersion =. val mbClientVersionText,
            PersonBundleVersion =. val mbBundleVersionText
          ]
        where_ $
          tbl ^. PersonTId ==. val (toKey person.id)

updateDeviceToken :: Id Person -> Maybe FCMRecipientToken -> SqlDB m ()
updateDeviceToken personId mbDeviceToken = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonUpdatedAt =. val now,
        PersonDeviceToken =. val mbDeviceToken
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB m ()
updateWhatsappNotificationEnrollStatus personId enrollStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

setIsNewFalse :: Id Person -> SqlDB m ()
setIsNewFalse personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
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
  Maybe (EncryptedHashed Text) ->
  Maybe FCMRecipientToken ->
  SqlDB m ()
updatePersonalInfo personId mbFirstName mbMiddleName mbLastName mbEncEmail mbDeviceToken = do
  now <- getCurrentTime
  let mbEmailEncrypted = mbEncEmail <&> unEncrypted . (.encrypted)
  let mbEmailHash = mbEncEmail <&> (.hash)
  Esq.update $ \tbl -> do
    set
      tbl
      ( [PersonUpdatedAt =. val now]
          <> updateWhenJust_ (\x -> PersonFirstName =. val (Just x)) mbFirstName
          <> updateWhenJust_ (\x -> PersonMiddleName =. val (Just x)) mbMiddleName
          <> updateWhenJust_ (\x -> PersonLastName =. val (Just x)) mbLastName
          <> updateWhenJust_ (\x -> PersonEmailEncrypted =. val (Just x)) mbEmailEncrypted
          <> updateWhenJust_ (\x -> PersonEmailHash =. val (Just x)) mbEmailHash
          <> updateWhenJust_ (\x -> PersonDeviceToken =. val (Just x)) mbDeviceToken
      )
    where_ $ tbl ^. PersonId ==. val (getId personId)

deleteById :: Id Person -> SqlDB m ()
deleteById personId = do
  Esq.delete $ do
    person <- from $ table @PersonT
    where_ (person ^. PersonId ==. val (getId personId))
