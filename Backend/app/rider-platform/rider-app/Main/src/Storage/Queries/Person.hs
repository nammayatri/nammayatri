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
import qualified Domain.Types.MerchantConfig as DMC
import Domain.Types.Person
import Domain.Types.Ride as Ride
import Kernel.External.Encryption
import Kernel.External.Maps (Language)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import Storage.Tabular.Booking
import Storage.Tabular.Person
import Storage.Tabular.Ride

create :: Person -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Person)
findById = Esq.findById

findByMobileNumberAndMerchantId ::
  Transactionable m =>
  Text ->
  DbHash ->
  Id Merchant ->
  m (Maybe Person)
findByMobileNumberAndMerchantId countryCode mobileNumberHash merchantId = do
  Esq.findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByEmailAndPassword ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByEmailAndPassword email_ password = do
  emailDbHash <- getDbHash email_
  passwordDbHash <- getDbHash password
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val (Just emailDbHash)
        &&. person ^. PersonPasswordHash ==. val (Just passwordDbHash)
    return person

findByEmail ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe Person)
findByEmail email_ = do
  emailDbHash <- getDbHash email_
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val (Just emailDbHash)
    return person

findByRoleAndMobileNumberAndMerchantId ::
  Transactionable m =>
  Role ->
  Text ->
  DbHash ->
  Id Merchant ->
  m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumberHash merchantId = do
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByRoleAndMobileNumberAndMerchantIdWithoutCC :: Transactionable m => Role -> DbHash -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantIdWithoutCC role_ mobileNumberHash merchantId = do
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

updateMultiple :: Id Person -> Person -> SqlDB ()
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

updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB ()
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

updateDeviceToken :: Id Person -> Maybe Text -> SqlDB ()
updateDeviceToken personId mbDeviceToken = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonUpdatedAt =. val now,
        PersonDeviceToken =. val mbDeviceToken
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB ()
updateWhatsappNotificationEnrollStatus personId enrollStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

setIsNewFalse :: Id Person -> SqlDB ()
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
  Maybe Text ->
  Maybe (EncryptedHashed Text) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Language ->
  Maybe Gender ->
  Maybe Version ->
  Maybe Version ->
  SqlDB ()
updatePersonalInfo personId mbFirstName mbMiddleName mbLastName mbReferralCode mbEncEmail mbDeviceToken mbNotificationToken mbLanguage mbGender mbCVersion mbBVersion = do
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
          <> updateWhenJust_ (\x -> PersonNotificationToken =. val (Just x)) mbNotificationToken
          <> updateWhenJust_ (\x -> PersonReferralCode =. val (Just x)) mbReferralCode
          <> updateWhenJust_ (\_ -> PersonReferredAt =. val (Just now)) mbReferralCode
          <> updateWhenJust_ (\x -> PersonLanguage =. val (Just x)) mbLanguage
          <> updateWhenJust_ (\x -> PersonGender =. val x) mbGender
          <> updateWhenJust_ (\x -> PersonClientVersion =. val (versionToText <$> Just x)) mbCVersion
          <> updateWhenJust_ (\x -> PersonBundleVersion =. val (versionToText <$> Just x)) mbBVersion
      )
    where_ $ tbl ^. PersonId ==. val (getId personId)

deleteById :: Id Person -> SqlDB ()
deleteById personId = do
  Esq.delete $ do
    person <- from $ table @PersonT
    where_ (person ^. PersonId ==. val (getId personId))

updateHasTakenValidRide :: Id Person -> SqlDB ()
updateHasTakenValidRide personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonHasTakenValidRide =. val True,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

updateReferralCodeAndReferredAt :: Id Person -> Maybe Text -> SqlDB ()
updateReferralCodeAndReferredAt personId referralCode = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonReferredAt =. val (Just now),
        PersonReferralCode =. val referralCode,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

findByReferralCode ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe Person)
findByReferralCode referralCode = do
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonReferralCode ==. val (Just referralCode)
    return person

findBlockedByDeviceToken :: Transactionable m => Maybe Text -> m [Person]
findBlockedByDeviceToken deviceToken = do
  findAll $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonDeviceToken ==. val deviceToken
        &&. person ^. PersonBlocked ==. val True
    return person

updateBlockedState :: Id Person -> Bool -> SqlDB ()
updateBlockedState personId isBlocked = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonBlocked =. val isBlocked,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonId ==. val (getId personId)

updatingEnabledAndBlockedState :: Id Person -> Maybe (Id DMC.MerchantConfig) -> Bool -> SqlDB ()
updatingEnabledAndBlockedState personId blockedByRule isBlocked = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      $ [ PersonEnabled =. val (not isBlocked),
          PersonBlocked =. val isBlocked,
          PersonBlockedByRuleId =. val (toKey <$> blockedByRule),
          PersonUpdatedAt =. val now
        ]
        <> [PersonBlockedAt =. val (Just now) | isBlocked]
    where_ $ tbl ^. PersonId ==. val (getId personId)

findAllCustomersById ::
  Transactionable m =>
  Id Merchant ->
  [Text] ->
  m [Person]
findAllCustomersById merchantId customerIdList = do
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMerchantId ==. (val . toKey $ merchantId)
        &&. person ^. PersonId `in_` valList customerIdList
    pure person

findAllCustomers ::
  Transactionable m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  m [Person]
findAllCustomers merchantId limitVal offsetVal mbEnabled mbBlocked mbSearchPhoneDBHash = do
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMerchantId ==. (val . toKey $ merchantId)
        &&. person ^. PersonRole ==. val USER
        &&. maybe (val True) (\enabled -> person ^. PersonEnabled ==. val enabled) mbEnabled
        &&. maybe (val True) (\blocked -> person ^. PersonBlocked ==. val blocked) mbBlocked
        &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
    orderBy [asc (person ^. PersonFirstName)]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    pure person

countCustomers :: Transactionable m => Id Merchant -> m Int
countCustomers merchantId =
  mkCount <$> do
    Esq.findAll $ do
      person <- from $ table @PersonT
      where_ $
        person ^. PersonMerchantId ==. val (toKey merchantId)
          &&. person ^. PersonRole ==. val USER
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

ridesCountAggTable :: SqlQuery (From (SqlExpr (Esq.Value PersonTId), SqlExpr (Esq.Value Int)))
ridesCountAggTable = with $ do
  ride :& booking <-
    from $
      table @RideT
        `innerJoin` table @BookingT
        `Esq.on` ( \(ride :& booking) ->
                     ride ^. RideBookingId ==. booking ^. BookingTId
                 )
  where_ (not_ $ ride ^. RideStatus `in_` valList [Ride.NEW, Ride.CANCELLED])
  groupBy $ booking ^. BookingRiderId
  pure (booking ^. BookingRiderId, count @Int $ ride ^. RideId)

fetchRidesCount :: Transactionable m => Id Person -> m (Maybe Int)
fetchRidesCount personId =
  join <$> do
    Esq.findOne $ do
      ridesCountAggQuery <- ridesCountAggTable
      person :& (_, mbRidesCount) <-
        from $
          table @PersonT
            `leftJoin` ridesCountAggQuery
            `Esq.on` ( \(person :& (mbPersonId, _mbRidesCount)) ->
                         just (person ^. PersonTId) ==. mbPersonId
                     )
      where_ $
        person ^. PersonTId ==. val (toKey personId)
          &&. person ^. PersonRole ==. val USER
      pure mbRidesCount
