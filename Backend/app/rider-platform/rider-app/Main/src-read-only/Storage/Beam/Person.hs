{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Person where

import qualified Data.Time
import qualified Database.Beam as B
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
  { id :: B.C f Kernel.Prelude.Text,
    firstName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    middleName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    lastName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    role :: B.C f Domain.Types.Person.Role,
    gender :: B.C f Domain.Types.Person.Gender,
    identifierType :: B.C f Domain.Types.Person.IdentifierType,
    emailEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    emailHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    unencryptedMobileNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    mobileCountryCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    passwordHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    identifier :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    totalRatings :: B.C f Kernel.Prelude.Int,
    totalRatingScore :: B.C f Kernel.Prelude.Int,
    isValidRating :: B.C f Kernel.Prelude.Bool,
    language :: B.C f (Kernel.Prelude.Maybe Kernel.External.Maps.Language),
    isNew :: B.C f Kernel.Prelude.Bool,
    enabled :: B.C f Kernel.Prelude.Bool,
    blocked :: B.C f Kernel.Prelude.Bool,
    deviceToken :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    notificationToken :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    currentCity :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    whatsappNotificationEnrollStatus :: B.C f (Kernel.Prelude.Maybe Kernel.External.Whatsapp.Interface.Types.OptApiMethods),
    referralCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    hasTakenValidRide :: B.C f Kernel.Prelude.Bool,
    hasDisability :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    blockedAt :: B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime),
    blockedByRuleId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadhaarVerified :: B.C f Kernel.Prelude.Bool,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    shareEmergencyContacts :: B.C f Kernel.Prelude.Bool,
    nightSafetyChecks :: B.C f Kernel.Prelude.Bool,
    shareTripWithEmergencyContactOption :: B.C f (Kernel.Prelude.Maybe Domain.Types.Person.RideShareOptions),
    hasCompletedMockSafetyDrill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasCompletedSafetySetup :: B.C f Kernel.Prelude.Bool,
    registrationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    registrationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    useFakeOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    followsRide :: B.C f Kernel.Prelude.Bool,
    falseSafetyAlarmCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    safetyCenterDisabledOnDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    referredByCustomer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    customerReferralCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blockedCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    registeredViaPartnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    customerPaymentId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.CustomerId),
    defaultPaymentMethodId :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f = PersonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonId . id

type Person = PersonT Identity

$(enableKVPG ''PersonT ['id] [['emailHash], ['mobileNumberHash], ['deviceToken], ['referralCode], ['customerReferralCode]])

$(mkTableInstances ''PersonT "person")
