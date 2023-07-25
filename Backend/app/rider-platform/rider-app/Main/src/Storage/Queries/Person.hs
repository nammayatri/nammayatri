{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person where

import Control.Applicative ((<|>))
import qualified Database.Beam as B
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.MerchantConfig as DMC
import Domain.Types.Person
import qualified Domain.Types.Ride as Ride
import qualified EulerHS.Language as L
import qualified EulerHS.Prelude as EP
import Kernel.External.Encryption
import Kernel.External.Maps (Language)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Version
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Ride as BeamR
import Storage.Tabular.Booking
import Storage.Tabular.Person
import Storage.Tabular.Ride

create :: (L.MonadFlow m, Log m) => Person -> m ()
create = createWithKV

-- findById ::
--   Transactionable m =>
--   Id Person ->
--   m (Maybe Person)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Person)
findById (Id personId) = findOneWithKV [Se.Is BeamP.id $ Se.Eq personId]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Person)
findByIdInReplica (Id personId) = findOneWithKvInReplica [Se.Is BeamP.id $ Se.Eq personId]

-- findByMobileNumberAndMerchantId ::
--   Transactionable m =>
--   Text ->
--   DbHash ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByMobileNumberAndMerchantId countryCode mobileNumberHash merchantId = do
--   Esq.findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonMobileCountryCode ==. val (Just countryCode)
--         &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByMobileNumberAndMerchantId :: (L.MonadFlow m, Log m) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchantId countryCode mobileNumberHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.mobileCountryCode $ Se.Eq (Just countryCode), Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByMobileNumberAndMerchantIdInReplica :: (L.MonadFlow m, Log m) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchantIdInReplica countryCode mobileNumberHash (Id merchantId) = findOneWithKvInReplica [Se.And [Se.Is BeamP.mobileCountryCode $ Se.Eq (Just countryCode), Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

-- findByEmailAndPassword ::
--   (Transactionable m, EncFlow m r) =>
--   Text ->
--   Text ->
--   m (Maybe Person)
-- findByEmailAndPassword email_ password = do
--   emailDbHash <- getDbHash email_
--   passwordDbHash <- getDbHash password
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonEmailHash ==. val (Just emailDbHash)
--         &&. person ^. PersonPasswordHash ==. val (Just passwordDbHash)
--     return person

findByEmailAndPassword :: (L.MonadFlow m, Log m, EncFlow m r) => Text -> Text -> m (Maybe Person)
findByEmailAndPassword email_ password = do
  emailDbHash <- getDbHash email_
  passwordDbHash <- getDbHash password
  findOneWithKV [Se.And [Se.Is BeamP.emailHash $ Se.Eq (Just emailDbHash), Se.Is BeamP.passwordHash $ Se.Eq (Just passwordDbHash)]]

-- findByEmail ::
--   (Transactionable m, EncFlow m r) =>
--   Text ->
--   m (Maybe Person)
-- findByEmail email_ = do
--   emailDbHash <- getDbHash email_
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonEmailHash ==. val (Just emailDbHash)
--     return person

findByEmail :: (L.MonadFlow m, Log m, EncFlow m r) => Text -> m (Maybe Person)
findByEmail email_ = do
  emailDbHash <- getDbHash email_
  findOneWithKV [Se.Is BeamP.emailHash $ Se.Eq (Just emailDbHash)]

-- findByRoleAndMobileNumberAndMerchantId ::
--   Transactionable m =>
--   Role ->
--   Text ->
--   DbHash ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumberHash merchantId = do
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonRole ==. val role_
--         &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
--         &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByRoleAndMobileNumberAndMerchantId :: (L.MonadFlow m, Log m) => Role -> Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumberHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.mobileCountryCode $ Se.Eq (Just countryCode), Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByRoleAndMobileNumberAndMerchantIdInReplica :: (L.MonadFlow m, Log m) => Role -> Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantIdInReplica role_ countryCode mobileNumberHash (Id merchantId) = findOneWithKvInReplica [Se.And [Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.mobileCountryCode $ Se.Eq (Just countryCode), Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

-- findByRoleAndMobileNumberAndMerchantIdWithoutCC :: Transactionable m => Role -> DbHash -> Id Merchant -> m (Maybe Person)
-- findByRoleAndMobileNumberAndMerchantIdWithoutCC role_ mobileNumberHash merchantId = do
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonRole ==. val role_
--         &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByRoleAndMobileNumberAndMerchantIdWithoutCC :: (L.MonadFlow m, Log m) => Role -> DbHash -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantIdWithoutCC role_ mobileNumberHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByRoleAndMobileNumberAndMerchantIdWithoutCCInReplica :: (L.MonadFlow m, Log m) => Role -> DbHash -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantIdWithoutCCInReplica role_ mobileNumberHash (Id merchantId) = findOneWithKvInReplica [Se.And [Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

-- updateMultiple :: Id Person -> Person -> SqlDB ()
-- updateMultiple personId person = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonUpdatedAt =. val now,
--         PersonFirstName =. val (person.firstName),
--         PersonMiddleName =. val (person.middleName),
--         PersonLastName =. val (person.lastName),
--         PersonGender =. val (person.gender),
--         PersonDescription =. val (person.description),
--         PersonRole =. val (person.role),
--         PersonIdentifier =. val (person.identifier),
--         PersonRating =. val (person.rating),
--         PersonDeviceToken =. val (person.deviceToken),
--         PersonClientVersion =. val (versionToText <$> person.clientVersion),
--         PersonBundleVersion =. val (versionToText <$> person.bundleVersion)
--       ]
--     where_ $ tbl ^. PersonId ==. val (getId personId)

updateMultiple :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Person -> m ()
updateMultiple (Id personId) person = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.updatedAt now,
      Se.Set BeamP.firstName $ person.firstName,
      Se.Set BeamP.middleName $ person.middleName,
      Se.Set BeamP.lastName $ person.lastName,
      Se.Set BeamP.gender $ person.gender,
      Se.Set BeamP.description $ person.description,
      Se.Set BeamP.role $ person.role,
      Se.Set BeamP.identifier $ person.identifier,
      Se.Set BeamP.rating $ person.rating,
      Se.Set BeamP.deviceToken $ person.deviceToken,
      Se.Set BeamP.clientVersion (versionToText <$> person.clientVersion),
      Se.Set BeamP.bundleVersion (versionToText <$> person.bundleVersion)
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

-- updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB ()
-- updatePersonVersions person mbBundleVersion mbClientVersion =
--   when
--     ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
--     do
--       now <- getCurrentTime
--       let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
--           mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
--       Esq.update $ \tbl -> do
--         set
--           tbl
--           [ PersonUpdatedAt =. val now,
--             PersonClientVersion =. val mbClientVersionText,
--             PersonBundleVersion =. val mbBundleVersionText
--           ]
--         where_ $
--           tbl ^. PersonTId ==. val (toKey person.id)

updatePersonVersions :: (L.MonadFlow m, MonadTime m, Log m) => Person -> Maybe Version -> Maybe Version -> m ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      updateWithKV
        [ Se.Set BeamP.updatedAt now,
          Se.Set BeamP.clientVersion mbClientVersionText,
          Se.Set BeamP.bundleVersion mbBundleVersionText
        ]
        [Se.Is BeamP.id (Se.Eq (getId (person.id)))]

-- updateDeviceToken :: Id Person -> Maybe Text -> SqlDB ()
-- updateDeviceToken personId mbDeviceToken = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonUpdatedAt =. val now,
--         PersonDeviceToken =. val mbDeviceToken
--       ]
--     where_ $ tbl ^. PersonId ==. val (getId personId)

updateDeviceToken :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Maybe Text -> m ()
updateDeviceToken (Id personId) mbDeviceToken = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.deviceToken mbDeviceToken,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

-- updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB ()
-- updateWhatsappNotificationEnrollStatus personId enrollStatus = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateWhatsappNotificationEnrollStatus :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Maybe Whatsapp.OptApiMethods -> m ()
updateWhatsappNotificationEnrollStatus (Id personId) enrollStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.whatsappNotificationEnrollStatus enrollStatus,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

-- setIsNewFalse :: Id Person -> SqlDB ()
-- setIsNewFalse personId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonUpdatedAt =. val now,
--         PersonIsNew =. val False
--       ]
--     where_ $ tbl ^. PersonId ==. val (getId personId)

setIsNewFalse :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m ()
setIsNewFalse (Id personId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.isNew False,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

-- updatePersonalInfo ::
--   Id Person ->
--   Maybe Text ->
--   Maybe Text ->
--   Maybe Text ->
--   Maybe Text ->
--   Maybe (EncryptedHashed Text) ->
--   Maybe Text ->
--   Maybe Text ->
--   Maybe Language ->
--   Maybe Gender ->
--   Maybe Version ->
--   Maybe Version ->
--   SqlDB ()
-- updatePersonalInfo personId mbFirstName mbMiddleName mbLastName mbReferralCode mbEncEmail mbDeviceToken mbNotificationToken mbLanguage mbGender mbCVersion mbBVersion = do
--   now <- getCurrentTime
--   let mbEmailEncrypted = mbEncEmail <&> unEncrypted . (.encrypted)
--   let mbEmailHash = mbEncEmail <&> (.hash)
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       ( [PersonUpdatedAt =. val now]
--           <> updateWhenJust_ (\x -> PersonFirstName =. val (Just x)) mbFirstName
--           <> updateWhenJust_ (\x -> PersonMiddleName =. val (Just x)) mbMiddleName
--           <> updateWhenJust_ (\x -> PersonLastName =. val (Just x)) mbLastName
--           <> updateWhenJust_ (\x -> PersonEmailEncrypted =. val (Just x)) mbEmailEncrypted
--           <> updateWhenJust_ (\x -> PersonEmailHash =. val (Just x)) mbEmailHash
--           <> updateWhenJust_ (\x -> PersonDeviceToken =. val (Just x)) mbDeviceToken
--           <> updateWhenJust_ (\x -> PersonNotificationToken =. val (Just x)) mbNotificationToken
--           <> updateWhenJust_ (\x -> PersonReferralCode =. val (Just x)) mbReferralCode
--           <> updateWhenJust_ (\_ -> PersonReferredAt =. val (Just now)) mbReferralCode
--           <> updateWhenJust_ (\x -> PersonLanguage =. val (Just x)) mbLanguage
--           <> updateWhenJust_ (\x -> PersonGender =. val x) mbGender
--           <> updateWhenJust_ (\x -> PersonClientVersion =. val (versionToText <$> Just x)) mbCVersion
--           <> updateWhenJust_ (\x -> PersonBundleVersion =. val (versionToText <$> Just x)) mbBVersion
--       )
--     where_ $ tbl ^. PersonId ==. val (getId personId)

updatePersonalInfo ::
  (L.MonadFlow m, MonadTime m, Log m) =>
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
  m ()
updatePersonalInfo (Id personId) mbFirstName mbMiddleName mbLastName mbReferralCode mbEncEmail mbDeviceToken mbNotificationToken mbLanguage mbGender mbCVersion mbBVersion = do
  now <- getCurrentTime
  let mbEmailEncrypted = mbEncEmail <&> unEncrypted . (.encrypted)
  let mbEmailHash = mbEncEmail <&> (.hash)
  updateWithKV
    ( [ Se.Set BeamP.updatedAt now
      ]
        <> if isJust mbFirstName
          then [Se.Set BeamP.firstName mbFirstName]
          else
            []
              <> if isJust mbMiddleName
                then [Se.Set BeamP.middleName mbMiddleName]
                else
                  []
                    <> if isJust mbLastName
                      then [Se.Set BeamP.lastName mbLastName]
                      else
                        []
                          <> if isJust mbEmailEncrypted
                            then [Se.Set BeamP.emailEncrypted mbEmailEncrypted]
                            else
                              []
                                <> if isJust mbEmailHash
                                  then [Se.Set BeamP.emailHash mbEmailHash]
                                  else
                                    []
                                      <> if isJust mbDeviceToken
                                        then [Se.Set BeamP.deviceToken mbDeviceToken]
                                        else
                                          []
                                            <> if isJust mbNotificationToken
                                              then [Se.Set BeamP.notificationToken mbNotificationToken]
                                              else
                                                []
                                                  <> if isJust mbReferralCode
                                                    then [Se.Set BeamP.referralCode mbReferralCode]
                                                    else
                                                      []
                                                        <> if isJust mbReferralCode
                                                          then [Se.Set BeamP.referredAt $ Just now]
                                                          else
                                                            []
                                                              <> if isJust mbLanguage
                                                                then [Se.Set BeamP.language mbLanguage]
                                                                else
                                                                  []
                                                                    <> maybe [] (\gender -> [Se.Set BeamP.gender gender]) mbGender
                                                                    <> if isJust mbCVersion
                                                                      then [Se.Set BeamP.clientVersion $ versionToText <$> mbCVersion]
                                                                      else
                                                                        []
                                                                          <> ([Se.Set BeamP.bundleVersion $ versionToText <$> mbBVersion | isJust mbBVersion])
    )
    [Se.Is BeamP.id (Se.Eq personId)]

-- deleteById :: Id Person -> SqlDB ()
-- deleteById personId = do
--   Esq.delete $ do
--     person <- from $ table @PersonT
--     where_ (person ^. PersonId ==. val (getId personId))

deleteById :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteById (Id personId) = deleteWithKV [Se.Is BeamP.id (Se.Eq personId)]

-- updateHasTakenValidRide :: Id Person -> SqlDB ()
-- updateHasTakenValidRide personId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonHasTakenValidRide =. val True,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonId ==. val (getId personId)

updateHasTakenValidRide :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m ()
updateHasTakenValidRide (Id personId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.hasTakenValidRide True,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

-- updateReferralCodeAndReferredAt :: Id Person -> Maybe Text -> SqlDB ()
-- updateReferralCodeAndReferredAt personId referralCode = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonReferredAt =. val (Just now),
--         PersonReferralCode =. val referralCode,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonId ==. val (getId personId)

updateReferralCodeAndReferredAt :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Maybe Text -> m ()
updateReferralCodeAndReferredAt (Id personId) referralCode = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.referredAt (Just now),
      Se.Set BeamP.referralCode referralCode,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

-- findByReferralCode ::
--   (Transactionable m, EncFlow m r) =>
--   Text ->
--   m (Maybe Person)
-- findByReferralCode referralCode = do
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonReferralCode ==. val (Just referralCode)
--     return person

findByReferralCode ::
  (L.MonadFlow m, EncFlow m r) =>
  Text ->
  m (Maybe Person)
findByReferralCode referralCode = findOneWithKV [Se.Is BeamP.referralCode (Se.Eq (Just referralCode))]

-- findBlockedByDeviceToken :: Transactionable m => Maybe Text -> m [Person]
-- findBlockedByDeviceToken deviceToken = do
--   findAll $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonDeviceToken ==. val deviceToken
--         &&. person ^. PersonBlocked ==. val True
--     return person

findBlockedByDeviceToken :: (L.MonadFlow m, EncFlow m r) => Maybe Text -> m [Person]
findBlockedByDeviceToken deviceToken = findAllWithKV [Se.And [Se.Is BeamP.deviceToken (Se.Eq deviceToken), Se.Is BeamP.blocked (Se.Eq True)]]

findBlockedByDeviceTokenInReplica :: (L.MonadFlow m, EncFlow m r) => Maybe Text -> m [Person]
findBlockedByDeviceTokenInReplica deviceToken = findAllWithKvInReplica [Se.And [Se.Is BeamP.deviceToken (Se.Eq deviceToken), Se.Is BeamP.blocked (Se.Eq True)]]

-- updateBlockedState :: Id Person -> Bool -> SqlDB ()
-- updateBlockedState personId isBlocked = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonBlocked =. val isBlocked,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonId ==. val (getId personId)

updateBlockedState :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Bool -> m ()
updateBlockedState (Id personId) isBlocked = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.blocked isBlocked,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

-- updatingEnabledAndBlockedState :: Id Person -> Maybe (Id DMC.MerchantConfig) -> Bool -> SqlDB ()
-- updatingEnabledAndBlockedState personId blockedByRule isBlocked = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       $ [ PersonEnabled =. val (not isBlocked),
--           PersonBlocked =. val isBlocked,
--           PersonBlockedByRuleId =. val (toKey <$> blockedByRule),
--           PersonUpdatedAt =. val now
--         ]
--         <> [PersonBlockedAt =. val (Just now) | isBlocked]
--     where_ $ tbl ^. PersonId ==. val (getId personId)

updatingEnabledAndBlockedState :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Maybe (Id DMC.MerchantConfig) -> Bool -> m ()
updatingEnabledAndBlockedState (Id personId) blockedByRule isBlocked = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set BeamP.enabled (not isBlocked),
        Se.Set BeamP.blocked isBlocked,
        Se.Set BeamP.blockedByRuleId $ getId <$> blockedByRule,
        Se.Set BeamP.updatedAt now
      ]
        <> [Se.Set BeamP.blockedAt (Just now) | isBlocked]
    )
    [Se.Is BeamP.id (Se.Eq personId)]

-- findAllCustomers ::
--   Transactionable m =>
--   Id Merchant ->
--   Int ->
--   Int ->
--   Maybe Bool ->
--   Maybe Bool ->
--   Maybe DbHash ->
--   m [Person]
-- findAllCustomers merchantId limitVal offsetVal mbEnabled mbBlocked mbSearchPhoneDBHash = do
--   Esq.findAll $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonMerchantId ==. (val . toKey $ merchantId)
--         &&. person ^. PersonRole ==. val USER
--         &&. maybe (val True) (\enabled -> person ^. PersonEnabled ==. val enabled) mbEnabled
--         &&. maybe (val True) (\blocked -> person ^. PersonBlocked ==. val blocked) mbBlocked
--         &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
--     orderBy [asc (person ^. PersonFirstName)]
--     limit $ fromIntegral limitVal
--     offset $ fromIntegral offsetVal
--     pure person

findAllCustomers :: (L.MonadFlow m, Log m) => Id Merchant -> Int -> Int -> Maybe Bool -> Maybe Bool -> Maybe DbHash -> m [Person]
findAllCustomers merchantId limitVal offsetVal mbEnabled mbBlocked mbSearchPhoneDBHash = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is BeamP.merchantId (Se.Eq (getId merchantId)),
            Se.Is BeamP.role (Se.Eq USER),
            Se.Is BeamP.enabled (maybe (Se.Eq True) Se.Eq mbEnabled),
            Se.Is BeamP.blocked (maybe (Se.Eq True) Se.Eq mbBlocked)
          ]
            <> ([Se.Is BeamP.mobileNumberHash $ Se.Eq mbSearchPhoneDBHash | isJust mbSearchPhoneDBHash])
        )
    ]
    (Se.Asc BeamP.firstName)
    (Just limitVal)
    (Just offsetVal)

findAllCustomersInReplica :: (L.MonadFlow m, Log m) => Id Merchant -> Int -> Int -> Maybe Bool -> Maybe Bool -> Maybe DbHash -> m [Person]
findAllCustomersInReplica merchantId limitVal offsetVal mbEnabled mbBlocked mbSearchPhoneDBHash = do
  findAllWithOptionsKvInReplica
    [ Se.And
        ( [ Se.Is BeamP.merchantId (Se.Eq (getId merchantId)),
            Se.Is BeamP.role (Se.Eq USER),
            Se.Is BeamP.enabled (maybe (Se.Eq True) Se.Eq mbEnabled),
            Se.Is BeamP.blocked (maybe (Se.Eq True) Se.Eq mbBlocked)
          ]
            <> ([Se.Is BeamP.mobileNumberHash $ Se.Eq mbSearchPhoneDBHash | isJust mbSearchPhoneDBHash])
        )
    ]
    (Se.Asc BeamP.firstName)
    (Just limitVal)
    (Just offsetVal)

-- countCustomers :: Transactionable m => Id Merchant -> m Int
-- countCustomers merchantId =
--   mkCount <$> do
--     Esq.findAll $ do
--       person <- from $ table @PersonT
--       where_ $
--         person ^. PersonMerchantId ==. val (toKey merchantId)
--           &&. person ^. PersonRole ==. val USER
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0

countCustomers :: (L.MonadFlow m, Log m) => Id Merchant -> m Int
countCustomers merchantId = findAllWithKV [Se.And [Se.Is BeamP.merchantId (Se.Eq (getId merchantId)), Se.Is BeamP.role (Se.Eq USER)]] <&> length

countCustomersInReplica :: (L.MonadFlow m, Log m) => Id Merchant -> m Int
countCustomersInReplica merchantId = findAllWithKvInReplica [Se.And [Se.Is BeamP.merchantId (Se.Eq (getId merchantId)), Se.Is BeamP.role (Se.Eq USER)]] <&> length

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

fetchRidesCount :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Int)
fetchRidesCount personId = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\(booking, _) -> (B.group_ (BeamB.riderId booking), B.as_ @Int B.countAll_)) $
          B.filter_'
            (\(_, ride) -> B.sqlBool_ (B.not_ (ride.status `B.in_` (B.val_ <$> [Ride.NEW, Ride.CANCELLED]))))
            do
              booking' <- B.all_ (BeamCommon.booking BeamCommon.atlasDB)
              ride' <- B.join_' (BeamCommon.ride BeamCommon.atlasDB) (\ride'' -> BeamR.bookingId ride'' B.==?. BeamB.id booking')
              pure (booking', ride')
  person <- findOneWithKV [Se.Is BeamP.id $ Se.Eq (getId personId)]
  let res' = either (const []) EP.id res
  maybe (pure Nothing) (\p -> pure (snd <$> find (\r -> (getId p.id) == fst r) res')) person

instance FromTType' BeamP.Person Person where
  fromTType' BeamP.PersonT {..} = do
    bundleVersion' <- forM bundleVersion readVersion
    clientVersion' <- forM clientVersion readVersion
    pure $
      Just $
        Person
          { id = Id id,
            firstName = firstName,
            middleName = middleName,
            lastName = lastName,
            role = role,
            gender = gender,
            identifierType = identifierType,
            email = EncryptedHashed <$> (Encrypted <$> emailEncrypted) <*> emailHash,
            unencryptedMobileNumber = unencryptedMobileNumber,
            mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
            mobileCountryCode = mobileCountryCode,
            passwordHash = passwordHash,
            identifier = identifier,
            rating = rating,
            language = language,
            isNew = isNew,
            enabled = enabled,
            blocked = blocked,
            deviceToken = deviceToken,
            notificationToken = notificationToken,
            description = description,
            merchantId = Id merchantId,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
            referralCode = referralCode,
            referredAt = referredAt,
            hasTakenValidRide = hasTakenValidRide,
            blockedAt = blockedAt,
            blockedByRuleId = Id <$> blockedByRuleId,
            createdAt = createdAt,
            updatedAt = updatedAt,
            bundleVersion = bundleVersion',
            clientVersion = clientVersion'
          }

instance ToTType' BeamP.Person Person where
  toTType' Person {..} = do
    BeamP.PersonT
      { BeamP.id = getId id,
        BeamP.firstName = firstName,
        BeamP.middleName = middleName,
        BeamP.lastName = lastName,
        BeamP.role = role,
        BeamP.gender = gender,
        BeamP.identifierType = identifierType,
        BeamP.emailEncrypted = email <&> unEncrypted . (.encrypted),
        BeamP.emailHash = email <&> (.hash),
        BeamP.unencryptedMobileNumber = unencryptedMobileNumber,
        BeamP.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        BeamP.mobileNumberHash = mobileNumber <&> (.hash),
        BeamP.mobileCountryCode = mobileCountryCode,
        BeamP.passwordHash = passwordHash,
        BeamP.identifier = identifier,
        BeamP.rating = rating,
        BeamP.language = language,
        BeamP.isNew = isNew,
        BeamP.enabled = enabled,
        BeamP.blocked = blocked,
        BeamP.deviceToken = deviceToken,
        BeamP.notificationToken = notificationToken,
        BeamP.description = description,
        BeamP.merchantId = getId merchantId,
        BeamP.whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
        BeamP.referralCode = referralCode,
        BeamP.referredAt = referredAt,
        BeamP.hasTakenValidRide = hasTakenValidRide,
        BeamP.blockedAt = blockedAt,
        BeamP.blockedByRuleId = getId <$> blockedByRuleId,
        BeamP.createdAt = createdAt,
        BeamP.updatedAt = updatedAt,
        BeamP.bundleVersion = versionToText <$> bundleVersion,
        BeamP.clientVersion = versionToText <$> clientVersion
      }
