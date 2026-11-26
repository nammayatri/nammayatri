{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.PlayIntegrity.Types
  ( DecodeIntegrityTokenReq (..),
    DecodeIntegrityTokenRes (..),
    TokenPayload (..),
    RequestDetails (..),
    AppIntegrity (..),
    DeviceIntegrity (..),
    RecentDeviceActivity (..),
    AccountDetails (..),
    EnvironmentDetails (..),
    AppAccessRiskVerdict (..),
    FlexibleInt (..),
  )
where

import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude

newtype DecodeIntegrityTokenReq = DecodeIntegrityTokenReq
  { integrityToken :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON DecodeIntegrityTokenReq where
  toJSON req = object ["integrity_token" .= req.integrityToken]

newtype DecodeIntegrityTokenRes = DecodeIntegrityTokenRes
  { tokenPayloadExternal :: TokenPayload
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype FlexibleInt = FlexibleInt {getFlexibleInt :: Integer}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FlexibleInt where
  parseJSON = \case
    String s -> maybe (fail "expected an integer string") (pure . FlexibleInt) $ readMaybe (T.unpack s)
    v -> FlexibleInt <$> parseJSON v

instance ToJSON FlexibleInt where
  toJSON = toJSON . (show :: Integer -> Text) . getFlexibleInt

data TokenPayload = TokenPayload
  { requestDetails :: Maybe RequestDetails,
    appIntegrity :: Maybe AppIntegrity,
    deviceIntegrity :: Maybe DeviceIntegrity,
    accountDetails :: Maybe AccountDetails,
    environmentDetails :: Maybe EnvironmentDetails
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RequestDetails = RequestDetails
  { requestPackageName :: Maybe Text,
    requestHash :: Maybe Text,
    timestampMillis :: Maybe FlexibleInt
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AppIntegrity = AppIntegrity
  { appRecognitionVerdict :: Maybe Text,
    packageName :: Maybe Text,
    certificateSha256Digest :: Maybe [Text],
    versionCode :: Maybe FlexibleInt
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DeviceIntegrity = DeviceIntegrity
  { deviceRecognitionVerdict :: Maybe [Text],
    recentDeviceActivity :: Maybe RecentDeviceActivity
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype RecentDeviceActivity = RecentDeviceActivity
  { deviceActivityLevel :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype AccountDetails = AccountDetails
  { appLicensingVerdict :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EnvironmentDetails = EnvironmentDetails
  { appAccessRiskVerdict :: Maybe AppAccessRiskVerdict,
    playProtectVerdict :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype AppAccessRiskVerdict = AppAccessRiskVerdict
  { appsDetected :: Maybe [Text]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
