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
import qualified Data.Time as T
import qualified Database.Beam as B
import qualified Domain.Types.Booking.Type as Booking
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.MerchantConfig as DMC
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps (Language)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Person as BeamP

create :: MonadFlow m => Person -> m ()
create = createWithKV

findById :: MonadFlow m => Id Person -> m (Maybe Person)
findById (Id personId) = findOneWithKV [Se.Is BeamP.id $ Se.Eq personId]

findByMobileNumberAndMerchantId :: MonadFlow m => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchantId countryCode mobileNumberHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.mobileCountryCode $ Se.Eq (Just countryCode), Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByEmailAndPassword :: (MonadFlow m, EncFlow m r) => Text -> Text -> m (Maybe Person)
findByEmailAndPassword email_ password = do
  emailDbHash <- getDbHash email_
  passwordDbHash <- getDbHash password
  findOneWithKV [Se.And [Se.Is BeamP.emailHash $ Se.Eq (Just emailDbHash), Se.Is BeamP.passwordHash $ Se.Eq (Just passwordDbHash)]]

findByEmail :: (MonadFlow m, EncFlow m r) => Text -> m (Maybe Person)
findByEmail email_ = do
  emailDbHash <- getDbHash email_
  findOneWithKV [Se.Is BeamP.emailHash $ Se.Eq (Just emailDbHash)]

findByRoleAndMobileNumberAndMerchantId :: MonadFlow m => Role -> Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumberHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.mobileCountryCode $ Se.Eq (Just countryCode), Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

findByRoleAndMobileNumberAndMerchantIdWithoutCC :: MonadFlow m => Role -> DbHash -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantIdWithoutCC role_ mobileNumberHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]

updateMultiple :: MonadFlow m => Id Person -> Person -> m ()
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
      Se.Set BeamP.deviceToken $ person.deviceToken,
      Se.Set BeamP.clientVersion (versionToText <$> person.clientVersion),
      Se.Set BeamP.bundleVersion (versionToText <$> person.bundleVersion)
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatePersonVersions :: MonadFlow m => Person -> Maybe Version -> Maybe Version -> m ()
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

updateAverageRating :: MonadFlow m => Id Person -> Int -> Int -> Bool -> m ()
updateAverageRating (Id personId) totalRatingsCount' totalRatingScore' isValidRating' = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set (\BeamP.PersonT {..} -> totalRatings) totalRatingsCount',
      Se.Set (\BeamP.PersonT {..} -> totalRatingScore) totalRatingScore',
      Se.Set BeamP.isValidRating isValidRating',
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateDeviceToken :: MonadFlow m => Id Person -> Maybe Text -> m ()
updateDeviceToken (Id personId) mbDeviceToken = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.deviceToken mbDeviceToken,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateWhatsappNotificationEnrollStatus :: MonadFlow m => Id Person -> Maybe Whatsapp.OptApiMethods -> m ()
updateWhatsappNotificationEnrollStatus (Id personId) enrollStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.whatsappNotificationEnrollStatus enrollStatus,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

setIsNewFalse :: MonadFlow m => Id Person -> m ()
setIsNewFalse (Id personId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.isNew False,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatePersonalInfo ::
  MonadFlow m =>
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
    ( [Se.Set BeamP.updatedAt now]
        <> [Se.Set BeamP.firstName mbFirstName | isJust mbFirstName]
        <> [Se.Set BeamP.middleName mbMiddleName | isJust mbMiddleName]
        <> [Se.Set BeamP.lastName mbLastName | isJust mbLastName]
        <> [Se.Set BeamP.emailEncrypted mbEmailEncrypted | isJust mbEmailEncrypted]
        <> [Se.Set BeamP.emailHash mbEmailHash | isJust mbEmailHash]
        <> [Se.Set BeamP.deviceToken mbDeviceToken | isJust mbDeviceToken]
        <> [Se.Set BeamP.notificationToken mbNotificationToken | isJust mbNotificationToken]
        <> [Se.Set BeamP.referralCode mbReferralCode | isJust mbReferralCode]
        <> [Se.Set BeamP.referredAt (Just now) | isJust mbReferralCode]
        <> [Se.Set BeamP.language mbLanguage | isJust mbLanguage]
        <> [Se.Set BeamP.gender (fromJust mbGender) | isJust mbGender]
        <> [Se.Set BeamP.clientVersion (versionToText <$> mbCVersion) | isJust mbCVersion]
        <> ([Se.Set BeamP.bundleVersion $ versionToText <$> mbBVersion | isJust mbBVersion])
    )
    [Se.Is BeamP.id (Se.Eq personId)]

deleteById :: MonadFlow m => Id Person -> m ()
deleteById (Id personId) = deleteWithKV [Se.Is BeamP.id (Se.Eq personId)]

updateHasTakenValidRide :: MonadFlow m => Id Person -> m ()
updateHasTakenValidRide (Id personId) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.hasTakenValidRide True,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateIsValidRating :: MonadFlow m => Id Person -> Bool -> m ()
updateIsValidRating (Id personId) isValidRating = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.isValidRating isValidRating,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateHasDisability :: MonadFlow m => Id Person -> Maybe Bool -> m ()
updateHasDisability (Id personId) mbHasDisability = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.hasDisability mbHasDisability,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updateReferralCodeAndReferredAt :: MonadFlow m => Id Person -> Maybe Text -> m ()
updateReferralCodeAndReferredAt (Id personId) referralCode = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.referredAt (Just now),
      Se.Set BeamP.referralCode referralCode,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

findByReferralCode ::
  (MonadFlow m, EncFlow m r) =>
  Text ->
  m (Maybe Person)
findByReferralCode referralCode = findOneWithKV [Se.Is BeamP.referralCode (Se.Eq (Just referralCode))]

findBlockedByDeviceToken :: (MonadFlow m, EncFlow m r) => Maybe Text -> m [Person]
findBlockedByDeviceToken Nothing = return [] -- return empty array in case device token is Nothing (WARNING: DON'T REMOVE IT)
findBlockedByDeviceToken deviceToken = findAllWithKV [Se.And [Se.Is BeamP.deviceToken (Se.Eq deviceToken), Se.Is BeamP.blocked (Se.Eq True)]]

updateBlockedState :: MonadFlow m => Id Person -> Bool -> m ()
updateBlockedState (Id personId) isBlocked = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.blocked isBlocked,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

updatingEnabledAndBlockedState :: MonadFlow m => Id Person -> Maybe (Id DMC.MerchantConfig) -> Bool -> m ()
updatingEnabledAndBlockedState (Id personId) blockedByRule isBlocked = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set BeamP.enabled (not isBlocked),
        Se.Set BeamP.blocked isBlocked,
        Se.Set BeamP.blockedByRuleId $ getId <$> blockedByRule,
        Se.Set BeamP.updatedAt now
      ]
        <> [Se.Set BeamP.blockedAt (Just $ T.utcToLocalTime T.utc now) | isBlocked]
    )
    [Se.Is BeamP.id (Se.Eq personId)]

findAllCustomers :: MonadFlow m => Id Merchant -> Int -> Int -> Maybe Bool -> Maybe Bool -> Maybe DbHash -> m [Person]
findAllCustomers merchantId limitVal offsetVal mbEnabled mbBlocked mbSearchPhoneDBHash = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is BeamP.merchantId (Se.Eq (getId merchantId)),
            Se.Is BeamP.role (Se.Eq USER)
          ]
            <> [Se.Is BeamP.enabled $ Se.Eq (fromJust mbEnabled) | isJust mbEnabled]
            <> [Se.Is BeamP.blocked $ Se.Eq (fromJust mbBlocked) | isJust mbBlocked]
            <> ([Se.Is BeamP.mobileNumberHash $ Se.Eq mbSearchPhoneDBHash | isJust mbSearchPhoneDBHash])
        )
    ]
    (Se.Asc BeamP.firstName)
    (Just limitVal)
    (Just offsetVal)

countCustomers :: MonadFlow m => Id Merchant -> m Int
countCustomers (Id merchantId) = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            (\person -> (BeamP.merchantId person B.==?. B.val_ merchantId) B.&&?. BeamP.role person B.==?. B.val_ USER)
            do
              B.all_ (BeamCommon.person BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res

fetchRidesCount :: MonadFlow m => Id Person -> m (Maybe Int)
fetchRidesCount personId = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            ( \booking ->
                B.sqlBool_ (booking.status `B.in_` (B.val_ <$> [Booking.COMPLETED, Booking.TRIP_ASSIGNED]))
                  B.&&?. booking.riderId B.==?. B.val_ (getId personId)
            )
            do
              B.all_ (BeamCommon.booking BeamCommon.atlasDB)
  pure $ either (const Nothing) (\r -> if null r then Nothing else Just (head r)) res

updateAadhaarVerifiedState :: MonadFlow m => Id Person -> Bool -> m ()
updateAadhaarVerifiedState (Id personId) isVerified = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamP.aadhaarVerified isVerified,
      Se.Set BeamP.updatedAt now
    ]
    [Se.Is BeamP.id (Se.Eq personId)]

instance FromTType' BeamP.Person Person where
  fromTType' BeamP.PersonT {..} = do
    bundleVersion' <- forM bundleVersion readVersion
    clientVersion' <- forM clientVersion readVersion
    pure $
      Just $
        Person
          { id = Id id,
            email = EncryptedHashed <$> (Encrypted <$> emailEncrypted) <*> emailHash,
            mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
            merchantId = Id merchantId,
            hasDisability = hasDisability,
            blockedAt = T.localTimeToUTC T.utc <$> blockedAt,
            blockedByRuleId = Id <$> blockedByRuleId,
            bundleVersion = bundleVersion',
            clientVersion = clientVersion',
            rating = Just $ fromIntegral totalRatingScore / fromIntegral totalRatings,
            ..
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
        BeamP.totalRatings = totalRatings,
        BeamP.totalRatingScore = totalRatingScore,
        BeamP.isValidRating = isValidRating,
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
        BeamP.hasDisability = hasDisability,
        BeamP.blockedAt = T.utcToLocalTime T.utc <$> blockedAt,
        BeamP.blockedByRuleId = getId <$> blockedByRuleId,
        BeamP.aadhaarVerified = aadhaarVerified,
        BeamP.createdAt = createdAt,
        BeamP.updatedAt = updatedAt,
        BeamP.bundleVersion = versionToText <$> bundleVersion,
        BeamP.clientVersion = versionToText <$> clientVersion
      }
