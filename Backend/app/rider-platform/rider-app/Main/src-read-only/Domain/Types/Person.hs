{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Person where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Whatsapp.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PersonE e = Person
  { id :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    role :: Domain.Types.Person.Role,
    gender :: Domain.Types.Person.Gender,
    identifierType :: Domain.Types.Person.IdentifierType,
    email :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    unencryptedMobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passwordHash :: Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash,
    identifier :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    totalRatings :: Kernel.Prelude.Int,
    totalRatingScore :: Kernel.Prelude.Int,
    isValidRating :: Kernel.Prelude.Bool,
    language :: Kernel.Prelude.Maybe Kernel.External.Maps.Language,
    isNew :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool,
    deviceToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    notificationToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    currentCity :: Kernel.Types.Beckn.Context.City,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    whatsappNotificationEnrollStatus :: Kernel.Prelude.Maybe Kernel.External.Whatsapp.Interface.Types.OptApiMethods,
    referralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    hasTakenValidRide :: Kernel.Prelude.Bool,
    hasDisability :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    blockedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    blockedByRuleId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantConfig.MerchantConfig),
    aadhaarVerified :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    shareEmergencyContacts :: Kernel.Prelude.Bool,
    nightSafetyChecks :: Kernel.Prelude.Bool,
    shareTripWithEmergencyContactOption :: Kernel.Prelude.Maybe Domain.Types.Person.RideShareOptions,
    hasCompletedMockSafetyDrill :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasCompletedSafetySetup :: Kernel.Prelude.Bool,
    registrationLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    registrationLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    useFakeOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    followsRide :: Kernel.Prelude.Bool,
    falseSafetyAlarmCount :: Kernel.Prelude.Int,
    safetyCenterDisabledOnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referredByCustomer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerReferralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockedCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    registeredViaPartnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    customerPaymentId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.CustomerId,
    defaultPaymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (entity, salt) = do
    email_ <- encryptItem $ (,salt) <$> email entity
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber entity
    pure
      Person
        { id = id entity,
          firstName = firstName entity,
          middleName = middleName entity,
          lastName = lastName entity,
          role = role entity,
          gender = gender entity,
          identifierType = identifierType entity,
          email = email_,
          unencryptedMobileNumber = unencryptedMobileNumber entity,
          mobileNumber = mobileNumber_,
          mobileCountryCode = mobileCountryCode entity,
          passwordHash = passwordHash entity,
          identifier = identifier entity,
          rating = rating entity,
          totalRatings = totalRatings entity,
          totalRatingScore = totalRatingScore entity,
          isValidRating = isValidRating entity,
          language = language entity,
          isNew = isNew entity,
          enabled = enabled entity,
          blocked = blocked entity,
          deviceToken = deviceToken entity,
          notificationToken = notificationToken entity,
          description = description entity,
          merchantId = merchantId entity,
          currentCity = currentCity entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus entity,
          referralCode = referralCode entity,
          referredAt = referredAt entity,
          hasTakenValidRide = hasTakenValidRide entity,
          hasDisability = hasDisability entity,
          blockedAt = blockedAt entity,
          blockedByRuleId = blockedByRuleId entity,
          aadhaarVerified = aadhaarVerified entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity,
          shareEmergencyContacts = shareEmergencyContacts entity,
          nightSafetyChecks = nightSafetyChecks entity,
          shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption entity,
          hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill entity,
          hasCompletedSafetySetup = hasCompletedSafetySetup entity,
          registrationLat = registrationLat entity,
          registrationLon = registrationLon entity,
          useFakeOtp = useFakeOtp entity,
          followsRide = followsRide entity,
          falseSafetyAlarmCount = falseSafetyAlarmCount entity,
          safetyCenterDisabledOnDate = safetyCenterDisabledOnDate entity,
          referredByCustomer = referredByCustomer entity,
          customerReferralCode = customerReferralCode entity,
          clientSdkVersion = clientSdkVersion entity,
          clientBundleVersion = clientBundleVersion entity,
          clientConfigVersion = clientConfigVersion entity,
          clientDevice = clientDevice entity,
          backendAppVersion = backendAppVersion entity,
          blockedCount = blockedCount entity,
          registeredViaPartnerOrgId = registeredViaPartnerOrgId entity,
          customerPaymentId = customerPaymentId entity,
          defaultPaymentMethodId = defaultPaymentMethodId entity
        }
  decryptItem entity = do
    email_ <- fmap fst <$> decryptItem (email entity)
    mobileNumber_ <- fmap fst <$> decryptItem (mobileNumber entity)
    pure
      ( Person
          { id = id entity,
            firstName = firstName entity,
            middleName = middleName entity,
            lastName = lastName entity,
            role = role entity,
            gender = gender entity,
            identifierType = identifierType entity,
            email = email_,
            unencryptedMobileNumber = unencryptedMobileNumber entity,
            mobileNumber = mobileNumber_,
            mobileCountryCode = mobileCountryCode entity,
            passwordHash = passwordHash entity,
            identifier = identifier entity,
            rating = rating entity,
            totalRatings = totalRatings entity,
            totalRatingScore = totalRatingScore entity,
            isValidRating = isValidRating entity,
            language = language entity,
            isNew = isNew entity,
            enabled = enabled entity,
            blocked = blocked entity,
            deviceToken = deviceToken entity,
            notificationToken = notificationToken entity,
            description = description entity,
            merchantId = merchantId entity,
            currentCity = currentCity entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus entity,
            referralCode = referralCode entity,
            referredAt = referredAt entity,
            hasTakenValidRide = hasTakenValidRide entity,
            hasDisability = hasDisability entity,
            blockedAt = blockedAt entity,
            blockedByRuleId = blockedByRuleId entity,
            aadhaarVerified = aadhaarVerified entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity,
            shareEmergencyContacts = shareEmergencyContacts entity,
            nightSafetyChecks = nightSafetyChecks entity,
            shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption entity,
            hasCompletedMockSafetyDrill = hasCompletedMockSafetyDrill entity,
            hasCompletedSafetySetup = hasCompletedSafetySetup entity,
            registrationLat = registrationLat entity,
            registrationLon = registrationLon entity,
            useFakeOtp = useFakeOtp entity,
            followsRide = followsRide entity,
            falseSafetyAlarmCount = falseSafetyAlarmCount entity,
            safetyCenterDisabledOnDate = safetyCenterDisabledOnDate entity,
            referredByCustomer = referredByCustomer entity,
            customerReferralCode = customerReferralCode entity,
            clientSdkVersion = clientSdkVersion entity,
            clientBundleVersion = clientBundleVersion entity,
            clientConfigVersion = clientConfigVersion entity,
            clientDevice = clientDevice entity,
            backendAppVersion = backendAppVersion entity,
            blockedCount = blockedCount entity,
            registeredViaPartnerOrgId = registeredViaPartnerOrgId entity,
            customerPaymentId = customerPaymentId entity,
            defaultPaymentMethodId = defaultPaymentMethodId entity
          },
        ""
      )

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data Gender = MALE | FEMALE | OTHER | UNKNOWN | PREFER_NOT_TO_SAY deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data IdentifierType = MOBILENUMBER | AADHAAR | EMAIL deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data RideShareOptions = ALWAYS_SHARE | SHARE_WITH_TIME_CONSTRAINTS | NEVER_SHARE deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data Role = USER | CUSTOMER_SUPPORT deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''Role)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''Role)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''IdentifierType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''IdentifierType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''Gender)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''Gender)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''RideShareOptions)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RideShareOptions)
