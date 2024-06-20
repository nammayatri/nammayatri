{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Person where

import qualified Data.Text
import qualified Data.Time
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Person as Beam
import qualified Storage.Queries.Transformers.Person

instance FromTType' Beam.Person Domain.Types.Person.Person where
  fromTType' (Beam.PersonT {..}) = do
    updateMerchantOpIdAndCity <- Storage.Queries.Transformers.Person.backfillCityAndMOCId currentCity merchantOperatingCityId merchantId
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    pure $
      Just
        Domain.Types.Person.Person
          { id = Kernel.Types.Id.Id id,
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
            rating = Just $ fromIntegral totalRatingScore / fromIntegral totalRatings,
            totalRatings = totalRatings,
            totalRatingScore = totalRatingScore,
            isValidRating = isValidRating,
            language = language,
            isNew = isNew,
            enabled = enabled,
            blocked = blocked,
            deviceToken = deviceToken,
            notificationToken = notificationToken,
            description = description,
            merchantId = Kernel.Types.Id.Id merchantId,
            currentCity = Kernel.Prelude.snd updateMerchantOpIdAndCity,
            merchantOperatingCityId = Kernel.Prelude.fst updateMerchantOpIdAndCity,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
            referralCode = referralCode,
            referredAt = referredAt,
            hasTakenValidRide = hasTakenValidRide,
            hasDisability = hasDisability,
            blockedAt = Data.Time.localTimeToUTC Data.Time.utc <$> blockedAt,
            blockedByRuleId = Kernel.Types.Id.Id <$> blockedByRuleId,
            aadhaarVerified = aadhaarVerified,
            createdAt = createdAt,
            updatedAt = updatedAt,
            shareEmergencyContacts = shareEmergencyContacts,
            nightSafetyChecks = nightSafetyChecks,
            shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption,
            hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
            hasCompletedSafetySetup = hasCompletedSafetySetup,
            registrationLat = registrationLat,
            registrationLon = registrationLon,
            useFakeOtp = useFakeOtp,
            followsRide = followsRide,
            falseSafetyAlarmCount = fromMaybe 0 falseSafetyAlarmCount,
            safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
            referredByCustomer = referredByCustomer,
            customerReferralCode = customerReferralCode,
            clientSdkVersion = clientSdkVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            backendAppVersion = backendAppVersion,
            blockedCount = blockedCount,
            registeredViaPartnerOrgId = Kernel.Types.Id.Id <$> registeredViaPartnerOrgId,
            customerPaymentId = customerPaymentId,
            defaultPaymentMethodId = defaultPaymentMethodId
          }

instance ToTType' Beam.Person Domain.Types.Person.Person where
  toTType' (Domain.Types.Person.Person {..}) = do
    Beam.PersonT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.firstName = firstName,
        Beam.middleName = middleName,
        Beam.lastName = lastName,
        Beam.role = role,
        Beam.gender = gender,
        Beam.identifierType = identifierType,
        Beam.emailEncrypted = email <&> unEncrypted . (.encrypted),
        Beam.emailHash = email <&> (.hash),
        Beam.unencryptedMobileNumber = unencryptedMobileNumber,
        Beam.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        Beam.mobileNumberHash = mobileNumber <&> (.hash),
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.passwordHash = passwordHash,
        Beam.identifier = identifier,
        Beam.totalRatings = totalRatings,
        Beam.totalRatingScore = totalRatingScore,
        Beam.isValidRating = isValidRating,
        Beam.language = language,
        Beam.isNew = isNew,
        Beam.enabled = enabled,
        Beam.blocked = blocked,
        Beam.deviceToken = deviceToken,
        Beam.notificationToken = notificationToken,
        Beam.description = description,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.currentCity = Kernel.Prelude.Just currentCity,
        Beam.merchantOperatingCityId = (Kernel.Prelude.Just . Kernel.Types.Id.getId) merchantOperatingCityId,
        Beam.whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
        Beam.referralCode = referralCode,
        Beam.referredAt = referredAt,
        Beam.hasTakenValidRide = hasTakenValidRide,
        Beam.hasDisability = hasDisability,
        Beam.blockedAt = Data.Time.utcToLocalTime Data.Time.utc <$> blockedAt,
        Beam.blockedByRuleId = Kernel.Types.Id.getId <$> blockedByRuleId,
        Beam.aadhaarVerified = aadhaarVerified,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.shareEmergencyContacts = shareEmergencyContacts,
        Beam.nightSafetyChecks = nightSafetyChecks,
        Beam.shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption,
        Beam.hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill,
        Beam.hasCompletedSafetySetup = hasCompletedSafetySetup,
        Beam.registrationLat = registrationLat,
        Beam.registrationLon = registrationLon,
        Beam.useFakeOtp = useFakeOtp,
        Beam.followsRide = followsRide,
        Beam.falseSafetyAlarmCount = Just falseSafetyAlarmCount,
        Beam.safetyCenterDisabledOnDate = safetyCenterDisabledOnDate,
        Beam.referredByCustomer = referredByCustomer,
        Beam.customerReferralCode = customerReferralCode,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.backendAppVersion = backendAppVersion,
        Beam.blockedCount = blockedCount,
        Beam.registeredViaPartnerOrgId = Kernel.Types.Id.getId <$> registeredViaPartnerOrgId,
        Beam.customerPaymentId = customerPaymentId,
        Beam.defaultPaymentMethodId = defaultPaymentMethodId
      }
