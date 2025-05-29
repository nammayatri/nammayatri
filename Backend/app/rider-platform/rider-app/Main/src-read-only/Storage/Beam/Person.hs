{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Person where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Person
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import qualified Kernel.External.Maps
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.External.Whatsapp.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data PersonT f = PersonT
  { aadhaarVerified :: B.C f Kernel.Prelude.Bool,
    androidId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    authBlocked :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blockSource :: B.C f (Kernel.Prelude.Maybe Domain.Types.Person.BlockSource),
    blocked :: B.C f Kernel.Prelude.Bool,
    blockedAt :: B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime),
    blockedByRuleId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blockedCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    blockedReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blockedUntil :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientReactNativeVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currentCity :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City),
    customerNammaTags :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    customerPaymentId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.CustomerId),
    customerReferralCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    dateOfBirth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    defaultPaymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    deviceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    deviceToken :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    emailEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    emailHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    enableOtpLessRide :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enabled :: B.C f Kernel.Prelude.Bool,
    falseSafetyAlarmCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    firstName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    followsRide :: B.C f Kernel.Prelude.Bool,
    frequentLocGeohashes :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    gender :: B.C f Domain.Types.Person.Gender,
    hasCompletedMockSafetyDrill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasCompletedSafetySetup :: B.C f Kernel.Prelude.Bool,
    hasDisability :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasTakenValidRide :: B.C f Kernel.Prelude.Bool,
    id :: B.C f Kernel.Prelude.Text,
    identifier :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    identifierType :: B.C f Domain.Types.Person.IdentifierType,
    imeiNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    imeiNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    informPoliceSos :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isNew :: B.C f Kernel.Prelude.Bool,
    isValidRating :: B.C f Kernel.Prelude.Bool,
    juspayCustomerPaymentID :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.CustomerId),
    language :: B.C f (Kernel.Prelude.Maybe Kernel.External.Maps.Language),
    lastName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    latestLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    latestLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    liveActivityToken :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    middleName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileCountryCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    nightSafetyChecks :: B.C f Kernel.Prelude.Bool,
    notificationToken :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    passwordHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    payoutVpa :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    profilePicture :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referralCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    referredByCustomer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    registeredViaPartnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    registrationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    registrationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    role :: B.C f Domain.Types.Person.Role,
    safetyCenterDisabledOnDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    shareEmergencyContacts :: B.C f Kernel.Prelude.Bool,
    shareTripWithEmergencyContactOption :: B.C f (Kernel.Prelude.Maybe Domain.Types.Person.RideShareOptions),
    totalRatingScore :: B.C f Kernel.Prelude.Int,
    totalRatings :: B.C f Kernel.Prelude.Int,
    totalRidesCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    useFakeOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    verificationChannel :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    whatsappNotificationEnrollStatus :: B.C f (Kernel.Prelude.Maybe Kernel.External.Whatsapp.Interface.Types.OptApiMethods)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f = PersonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonId . id

type Person = PersonT Identity

$(enableKVPG ''PersonT ['id] [['customerReferralCode], ['deviceToken], ['emailHash], ['mobileNumberHash], ['referralCode]])

$(mkTableInstances ''PersonT "person")
