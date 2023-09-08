{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Person where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.FraudConfig as DFC
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common (Centesimal, maskText)
import Servant.API

data Role
  = USER
  | CUSTOMER_SUPPORT
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON, Enum, Bounded, ToSchema)

$(mkBeamInstancesForEnum ''Role)

instance FromHttpApiData Role where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData Role where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

-------------------------------------------------------------------------------------------
data IdentifierType = MOBILENUMBER | AADHAAR | EMAIL
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

$(mkBeamInstancesForEnum ''IdentifierType)

instance FromHttpApiData IdentifierType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData IdentifierType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

--------------------------------------------------------------------------------------------------
data Gender = MALE | FEMALE | OTHER | UNKNOWN | PREFER_NOT_TO_SAY
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''Gender)

instance FromHttpApiData Gender where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

--------------------------------------------------------------------------------------------------

data PersonE e = Person
  { id :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    role :: Role,
    gender :: Gender,
    identifierType :: IdentifierType,
    email :: Maybe (EncryptedHashedField e Text),
    unencryptedMobileNumber :: Maybe Text,
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    mobileCountryCode :: Maybe Text,
    passwordHash :: Maybe DbHash,
    identifier :: Maybe Text,
    rating :: Maybe Centesimal,
    totalRatings :: Int,
    totalRatingScore :: Int,
    isValidRating :: Bool,
    language :: Maybe Maps.Language,
    isNew :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text,
    description :: Maybe Text,
    merchantId :: Id DMerchant.Merchant,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    referralCode :: Maybe Text,
    referredAt :: Maybe UTCTime,
    hasTakenValidRide :: Bool,
    hasDisability :: Maybe Bool,
    blockedAt :: Maybe UTCTime,
    blockedByRuleId :: Maybe (Id DFC.FraudConfig),
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (Person {..}, salt) = do
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber
    email_ <- encryptItem $ (,salt) <$> email
    return Person {mobileNumber = mobileNumber_, email = email_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fmap fst <$> decryptItem mobileNumber
    email_ <- fmap fst <$> decryptItem email
    return (Person {mobileNumber = mobileNumber_, email = email_, ..}, "")

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe Text,
    hasTakenRide :: Bool,
    hasTakenValidRide :: Bool,
    referralCode :: Maybe Text,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    language :: Maybe Maps.Language,
    hasDisability :: Maybe Bool,
    disability :: Maybe Text,
    gender :: Gender,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> Maybe Text -> PersonAPIEntity
makePersonAPIEntity Person {..} disability =
  PersonAPIEntity
    { maskedMobileNumber = maskText <$> mobileNumber,
      maskedDeviceToken = maskText <$> deviceToken,
      hasTakenRide = hasTakenValidRide,
      ..
    }
