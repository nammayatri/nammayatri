{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Person where

import Beckn.External.Encryption
import qualified Beckn.External.FCM.Types as FCM
import Beckn.External.Types (Language)
import Beckn.Types.Id
import Beckn.Types.Version
import Beckn.Utils.Common (Centesimal, EsqDBFlow, maskText)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id)
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
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    mobileCountryCode :: Maybe Text,
    passwordHash :: Maybe DbHash,
    identifier :: Maybe Text,
    rating :: Maybe Centesimal,
    isNew :: Bool,
    merchantId :: Id DM.Merchant,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Language,
    description :: Maybe Text,
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
    return Person {mobileNumber = mobileNumber_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fmap fst <$> decryptItem mobileNumber
    return (Person {mobileNumber = mobileNumber_, ..}, "")

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
    role :: Role
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
