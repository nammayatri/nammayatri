{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Domain.Types.AppAttestationConfig
  ( AppAttestationConfig (..),
    FallbackConfig (..),
    defaultAppAttestationConfig,
    defaultFallbackConfig,
  )
where

import AppAttestation.Domain.Types.Common
import Data.Aeson
import Kernel.Prelude

data AppAttestationConfig = AppAttestationConfig
  { mode :: AttestationMode,
    onInvalid :: AttestationAction,
    onMissing :: AttestationAction,
    onUnsupportedOrError :: AttestationAction,
    minAndroidDeviceVerdict :: AndroidDeviceVerdict,
    requireAndroidPlayRecognizedApp :: Bool,
    requireAndroidLicensedApp :: Bool,
    maxAndroidRecentDeviceActivityLevel :: Maybe Int,
    rejectedAndroidAppAccessRisks :: [Text],
    androidApps :: [AndroidAppConfig],
    iosApps :: [IosAppConfig],
    minAndroidAttestingAppVersion :: Maybe Text,
    minIosAttestingAppVersion :: Maybe Text,
    appAttestRootCertPem :: Maybe Text,
    tokenMaxAgeSeconds :: Int,
    platformTimeoutMs :: Int,
    maxOtpPerIosKeyPerDay :: Maybe Int,
    fallback :: FallbackConfig
  }
  deriving (Show, Eq, Generic, ToJSON)

data FallbackConfig = FallbackConfig
  { methods :: [FallbackMethod],
    maxOtpPerPhonePerHour :: Int,
    maxOtpPerIpPerHour :: Int,
    captchaMinScore :: Maybe Double,
    captchaAllowedHostnames :: Maybe [Text]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON AppAttestationConfig where
  parseJSON = withObject "AppAttestationConfig" $ \o -> do
    let d = defaultAppAttestationConfig
    mode <- o .:? "mode" .!= d.mode
    onInvalid <- o .:? "onInvalid" .!= d.onInvalid
    onMissing <- o .:? "onMissing" .!= d.onMissing
    onUnsupportedOrError <- o .:? "onUnsupportedOrError" .!= d.onUnsupportedOrError
    minAndroidDeviceVerdict <- o .:? "minAndroidDeviceVerdict" .!= d.minAndroidDeviceVerdict
    requireAndroidPlayRecognizedApp <- o .:? "requireAndroidPlayRecognizedApp" .!= d.requireAndroidPlayRecognizedApp
    requireAndroidLicensedApp <- o .:? "requireAndroidLicensedApp" .!= d.requireAndroidLicensedApp
    maxAndroidRecentDeviceActivityLevel <- o .:? "maxAndroidRecentDeviceActivityLevel"
    rejectedAndroidAppAccessRisks <- o .:? "rejectedAndroidAppAccessRisks" .!= d.rejectedAndroidAppAccessRisks
    androidApps <- o .:? "androidApps" .!= d.androidApps
    iosApps <- o .:? "iosApps" .!= d.iosApps
    minAndroidAttestingAppVersion <- o .:? "minAndroidAttestingAppVersion"
    minIosAttestingAppVersion <- o .:? "minIosAttestingAppVersion"
    appAttestRootCertPem <- o .:? "appAttestRootCertPem"
    tokenMaxAgeSeconds <- o .:? "tokenMaxAgeSeconds" .!= d.tokenMaxAgeSeconds
    platformTimeoutMs <- o .:? "platformTimeoutMs" .!= d.platformTimeoutMs
    maxOtpPerIosKeyPerDay <- o .:? "maxOtpPerIosKeyPerDay"
    fallback <- o .:? "fallback" .!= d.fallback
    pure AppAttestationConfig {..}

instance FromJSON FallbackConfig where
  parseJSON = withObject "FallbackConfig" $ \o -> do
    let d = defaultFallbackConfig
    methods <- o .:? "methods" .!= d.methods
    maxOtpPerPhonePerHour <- o .:? "maxOtpPerPhonePerHour" .!= d.maxOtpPerPhonePerHour
    maxOtpPerIpPerHour <- o .:? "maxOtpPerIpPerHour" .!= d.maxOtpPerIpPerHour
    captchaMinScore <- o .:? "captchaMinScore"
    captchaAllowedHostnames <- o .:? "captchaAllowedHostnames"
    pure FallbackConfig {..}

defaultAppAttestationConfig :: AppAttestationConfig
defaultAppAttestationConfig =
  AppAttestationConfig
    { mode = OFF,
      onInvalid = SILENT_DROP,
      onMissing = ALLOW,
      onUnsupportedOrError = FALLBACK,
      minAndroidDeviceVerdict = MEETS_DEVICE_INTEGRITY,
      requireAndroidPlayRecognizedApp = True,
      requireAndroidLicensedApp = False,
      maxAndroidRecentDeviceActivityLevel = Nothing,
      rejectedAndroidAppAccessRisks = [],
      androidApps = [],
      iosApps = [],
      minAndroidAttestingAppVersion = Nothing,
      minIosAttestingAppVersion = Nothing,
      appAttestRootCertPem = Nothing,
      tokenMaxAgeSeconds = 600,
      platformTimeoutMs = 2000,
      maxOtpPerIosKeyPerDay = Nothing,
      fallback = defaultFallbackConfig
    }

defaultFallbackConfig :: FallbackConfig
defaultFallbackConfig =
  FallbackConfig
    { methods = [WHATSAPP_OTP],
      maxOtpPerPhonePerHour = 2,
      maxOtpPerIpPerHour = 10,
      captchaMinScore = Nothing,
      captchaAllowedHostnames = Nothing
    }
