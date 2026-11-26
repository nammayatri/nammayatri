{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AppAttestation.Domain.Types.Common
  ( Merchant,
    MerchantOperatingCity,
    AttestationMode (..),
    AttestationAction (..),
    AndroidDeviceVerdict (..),
    AppAttestEnvironment (..),
    AppAttestKeyStatus (..),
    FallbackMethod (..),
    AndroidAppConfig (..),
    IosAppConfig (..),
    AppAttestationProvider (..),
    CaptchaProvider (..),
    PlayIntegrityConfig (..),
    CaptchaConfig (..),
  )
where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.External.Encryption (EncKind (..), EncryptedField)
import Kernel.Prelude

data Merchant

data MerchantOperatingCity

data AttestationMode
  = OFF
  | SHADOW
  | ENFORCE_NEW_USERS
  | ENFORCE_ALL
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AttestationAction
  = ALLOW
  | FALLBACK
  | SILENT_DROP
  | REJECT
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AndroidDeviceVerdict
  = MEETS_BASIC_INTEGRITY
  | MEETS_DEVICE_INTEGRITY
  | MEETS_STRONG_INTEGRITY
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AppAttestEnvironment
  = PRODUCTION
  | DEVELOPMENT
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AppAttestKeyStatus
  = ACTIVE
  | REVOKED
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data FallbackMethod
  = WHATSAPP_OTP
  | CAPTCHA
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AndroidAppConfig = AndroidAppConfig
  { packageName :: Text,
    certSha256Digests :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data IosAppConfig = IosAppConfig
  { teamId :: Text,
    bundleId :: Text,
    environment :: AppAttestEnvironment
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AppAttestationProvider = PlayIntegrity
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CaptchaProvider = SiteVerify
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PlayIntegrityConfig = PlayIntegrityConfig
  { url :: BaseUrl,
    serviceAccountJson :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data CaptchaConfig = CaptchaConfig
  { verifyUrl :: BaseUrl,
    secret :: EncryptedField 'AsEncrypted Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

$(mkBeamInstancesForEnumAndList ''AttestationMode)

$(mkBeamInstancesForEnumAndList ''AttestationAction)

$(mkBeamInstancesForEnumAndList ''AndroidDeviceVerdict)

$(mkBeamInstancesForEnumAndList ''AppAttestEnvironment)

$(mkBeamInstancesForEnumAndList ''AppAttestKeyStatus)
