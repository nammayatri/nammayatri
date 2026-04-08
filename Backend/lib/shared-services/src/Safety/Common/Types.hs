module Safety.Common.Types
  ( SafetyCtx (..),
    SafetySettingsPersonDefaults (..),
    emptyPersonDefaults,
    PersonE (..),
  )
where

import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Safety.Domain.Types.Common as Common

-- | Uniform downstream state produced by the Builder.
-- All shared domain functions consume this — no platform-specific types leak in.
data SafetyCtx = SafetyCtx
  { personId :: Id Common.Person,
    merchantId :: Id Common.Merchant,
    merchantOpCityId :: Maybe (Id Common.MerchantOperatingCity),
    personDefaults :: SafetySettingsPersonDefaults
  }

-- | Rider's Person has safety fields inline; driver's Person has none.
-- This record normalises the difference.
data SafetySettingsPersonDefaults = SafetySettingsPersonDefaults
  { falseSafetyAlarmCount :: Maybe Int,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    hasCompletedSafetySetup :: Maybe Bool,
    nightSafetyChecks :: Maybe Bool,
    safetyCenterDisabledOnDate :: Maybe UTCTime,
    shareEmergencyContacts :: Maybe Bool,
    informPoliceSos :: Maybe Bool
  }
  deriving (Generic, Show)

emptyPersonDefaults :: SafetySettingsPersonDefaults
emptyPersonDefaults =
  SafetySettingsPersonDefaults
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- | Narrow person type — shared lib never imports app-specific Person
data PersonE = PersonE
  { id :: Id Common.Person,
    merchantId :: Id Common.Merchant,
    merchantOpCityId :: Id Common.MerchantOperatingCity,
    mobileNumber :: Maybe Text,
    name :: Maybe Text,
    language :: Maybe Language
  }
  deriving (Generic, Show)
