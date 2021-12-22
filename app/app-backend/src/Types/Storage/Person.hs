{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Person where

import Beckn.External.Encryption
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Types.Id
import Beckn.Utils.Common (maskText)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.OpenApi as OA
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API
import qualified Types.Storage.Organization as Org

data Role
  = USER
  | CUSTOMER_SUPPORT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, Enum, Bounded, OA.ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Role where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Role where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Role

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

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IdentifierType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be IdentifierType

instance FromBackendRow Postgres IdentifierType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

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
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, OA.ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Gender where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Gender where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData Gender where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data PersonTE e f = Person
  { id :: B.C f (Id Person),
    firstName :: B.C f (Maybe Text),
    middleName :: B.C f (Maybe Text),
    lastName :: B.C f (Maybe Text),
    fullName :: B.C f (Maybe Text),
    role :: B.C f Role,
    gender :: B.C f Gender,
    identifierType :: B.C f IdentifierType,
    email :: B.C f (Maybe Text),
    mobileNumber :: EncryptedHashedField e (B.Nullable f) Text,
    mobileCountryCode :: B.C f (Maybe Text),
    passwordHash :: B.C f (Maybe DbHash),
    identifier :: B.C f (Maybe Text),
    rating :: B.C f (Maybe Text),
    isNew :: B.C f Bool,
    udf1 :: B.C f (Maybe Text),
    udf2 :: B.C f (Maybe Text),
    organizationId :: B.C f (Maybe (Id Org.Organization)),
    deviceToken :: B.C f (Maybe FCM.FCMRecipientToken),
    description :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic)

type Person = PersonT Identity

type PersonT = PersonTE 'AsEncrypted

type DecryptedPerson = PersonTE 'AsUnencrypted Identity

instance ToJSON DecryptedPerson

instance FromJSON DecryptedPerson

instance B.Beamable PersonT

type PersonPrimaryKey = B.PrimaryKey PersonT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table PersonT where
  data PrimaryKey PersonT f = PersonPrimaryKey (B.C f (Id Person))
    deriving (Generic, B.Beamable)
  primaryKey t = PersonPrimaryKey t.id

deriveTableEncryption ''PersonTE

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity PersonT)
fieldEMod =
  B.modifyTableFields
    (B.tableModification @_ @PersonT)
      { createdAt = "created_at",
        updatedAt = "updated_at",
        firstName = "first_name",
        middleName = "middle_name",
        lastName = "last_name",
        fullName = "full_name",
        passwordHash = "password_hash",
        isNew = "is_new",
        mobileNumber =
          EncryptedHashed
            { encrypted = "mobile_number_encrypted",
              hash = "mobile_number_hash"
            },
        organizationId = "organization_id",
        mobileCountryCode = "mobile_country_code",
        identifierType = "identifier_type",
        deviceToken = "device_token"
      }

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: FCM.FCMRecipientToken
  }
  deriving (Generic, Show, FromJSON, ToJSON, OA.ToSchema)

makePersonAPIEntity :: DecryptedPerson -> PersonAPIEntity
makePersonAPIEntity Person {..} =
  PersonAPIEntity
    { maskedMobileNumber = maskText <$> mobileNumber,
      maskedDeviceToken = maybe (FCM.FCMRecipientToken "...") (FCM.FCMRecipientToken . maskText . (.getFCMRecipientToken)) deviceToken,
      ..
    }
