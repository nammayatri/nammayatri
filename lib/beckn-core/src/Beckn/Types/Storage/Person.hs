{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Person where

import Beckn.External.Encryption
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Organization as Org
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger hiding (description, email)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API

data Status = ACTIVE | INACTIVE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToSchema Status

instance ToParamSchema Status

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Status

-------------------------------------------------------------------------------------------

data Role
  = USER
  | DRIVER
  | ADMIN
  | VALIDATOR
  | MANAGER
  | VIEWER
  | WARDLEVEL
  | DISTRICTLEVEL
  | CITYLEVEL
  | STATELEVEL
  | CUSTOMER_SUPPORT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, Enum, Bounded)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Role where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Role where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Role

instance ToParamSchema Role

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

instance ToSchema IdentifierType

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
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Gender where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Gender where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Gender

instance FromHttpApiData Gender where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data PersonT f = Person
  { id :: B.C f (Id Person),
    firstName :: B.C f (Maybe Text),
    middleName :: B.C f (Maybe Text),
    lastName :: B.C f (Maybe Text),
    fullName :: B.C f (Maybe Text),
    role :: B.C f Role,
    gender :: B.C f Gender,
    identifierType :: B.C f IdentifierType,
    email :: B.C f (Maybe Text),
    mobileNumber :: EncryptedHashedField 'AsEncrypted (B.Nullable f) Text,
    mobileCountryCode :: B.C f (Maybe Text),
    passwordHash :: B.C f (Maybe DbHash),
    identifier :: B.C f (Maybe Text),
    rating :: B.C f (Maybe Text),
    verified :: B.C f Bool,
    udf1 :: B.C f (Maybe Text),
    udf2 :: B.C f (Maybe Text),
    status :: B.C f Status,
    organizationId :: B.C f (Maybe (Id Org.Organization)),
    locationId :: B.C f (Maybe (Id Loc.Location)),
    deviceToken :: B.C f (Maybe FCM.FCMRecipientToken),
    description :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic)

type Person = PersonT Identity

instance B.Beamable PersonT

type PersonPrimaryKey = B.PrimaryKey PersonT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table PersonT where
  data PrimaryKey PersonT f = PersonPrimaryKey (B.C f (Id Person))
    deriving (Generic, B.Beamable)
  primaryKey t = PersonPrimaryKey t.id

-- TODO: move it to appropriate place
maskPerson :: DecryptedPerson -> DecryptedPerson
maskPerson person =
  person {deviceToken = FCM.FCMRecipientToken . trimToken . FCM.getFCMRecipientToken <$> (person.deviceToken)}
  where
    trimToken token =
      if length token > 6
        then T.take 3 token <> "..." <> T.takeEnd 3 token
        else "..."

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity PersonT)
fieldEMod =
  B.modifyTableFields @PersonT $
    B.tableModification
      { createdAt = "created_at",
        updatedAt = "updated_at",
        firstName = "first_name",
        middleName = "middle_name",
        lastName = "last_name",
        fullName = "full_name",
        passwordHash = "password_hash",
        mobileNumber =
          EncryptedHashed
            { encrypted = "mobile_number_encrypted",
              hash = "mobile_number_hash"
            },
        organizationId = "organization_id",
        locationId = "location_id",
        mobileCountryCode = "mobile_country_code",
        identifierType = "identifier_type",
        deviceToken = "device_token"
      }

data DecryptedPerson = DecryptedPerson
  { id :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: Role,
    gender :: Gender,
    identifierType :: IdentifierType,
    email :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    passwordHash :: Maybe DbHash,
    identifier :: Maybe Text,
    rating :: Maybe Text,
    verified :: Bool,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    status :: Status,
    organizationId :: Maybe (Id Org.Organization),
    locationId :: Maybe (Id Loc.Location),
    deviceToken :: Maybe FCM.FCMRecipientToken,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON)

buildDecryptedPerson :: HasFlowEncEnv m r => Person -> m DecryptedPerson
buildDecryptedPerson Person {..} = do
  decMobileNumber <- decrypt mobileNumber
  return $
    DecryptedPerson
      { mobileNumber = decMobileNumber,
        ..
      }
