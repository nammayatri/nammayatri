{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Person where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.Maps as Maps
import Kernel.External.Types (Language)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common (Centesimal, EsqDBFlow, maskText)
import Servant.API

data Driver -- = Person

data Role
  = DRIVER
  | ADMIN
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, Enum, Bounded, ToSchema)

instance FromHttpApiData Role where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData Role where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

-------------------------------------------------------------------------------------------
data IdentifierType = MOBILENUMBER | AADHAAR | EMAIL
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData IdentifierType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData IdentifierType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

--------------------------------------------------------------------------------------------------
data Gender = MALE | FEMALE | OTHER | UNKNOWN
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData Gender where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data PersonE e = Person
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    role :: Role,
    gender :: Gender,
    identifierType :: IdentifierType,
    email :: Maybe Text,
    unencryptedMobileNumber :: Maybe Text,
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    mobileCountryCode :: Maybe Text,
    passwordHash :: Maybe DbHash,
    identifier :: Maybe Text,
    rating :: Maybe Centesimal,
    isNew :: Bool,
    merchantId :: Id DM.Merchant,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    language :: Maybe Language,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    unencryptedAlternateMobileNumber :: Maybe Text,
    alternateMobileNumber :: Maybe (EncryptedHashedField e Text)
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (Person {..}, salt) = do
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber
    alternateMobileNumber_ <- encryptItem $ (,salt) <$> alternateMobileNumber
    return Person {mobileNumber = mobileNumber_, alternateMobileNumber = alternateMobileNumber_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fmap fst <$> decryptItem mobileNumber
    alternateMobileNumber_ <- fmap fst <$> decryptItem alternateMobileNumber
    return (Person {mobileNumber = mobileNumber_, alternateMobileNumber = alternateMobileNumber_, ..}, "")

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCM.FCMRecipientToken,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    role :: Role,
    language :: Maybe Maps.Language
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> PersonAPIEntity
makePersonAPIEntity Person {..} =
  PersonAPIEntity
    { maskedMobileNumber = maskText <$> mobileNumber,
      maskedDeviceToken = FCM.FCMRecipientToken . maskText . (.getFCMRecipientToken) <$> deviceToken,
      ..
    }

getPersonNumber :: (EncFlow m r) => Person -> m (Maybe Text)
getPersonNumber person = do
  decMobileNumber <- mapM decrypt person.mobileNumber
  return $ person.mobileCountryCode <> decMobileNumber

getPersonFullName :: (EsqDBFlow m r, EncFlow m r) => Person -> m (Maybe Text)
getPersonFullName person = do
  return ((\fN -> fN <> maybe "" (" " <>) person.lastName) <$> Just person.firstName)
