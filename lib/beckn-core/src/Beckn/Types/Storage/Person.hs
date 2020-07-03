{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Person where

import Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import Servant.API
import Servant.Swagger

data Status = ACTIVE | INACTIVE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToSchema Status

instance ToParamSchema Status

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
data IdentifierType = MOBILENUMBER | AADHAAR
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IdentifierType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be IdentifierType

instance FromBackendRow Postgres IdentifierType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToSchema IdentifierType

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
  { _id :: B.C f PersonId,
    _firstName :: B.C f (Maybe Text),
    _middleName :: B.C f (Maybe Text),
    _lastName :: B.C f (Maybe Text),
    _fullName :: B.C f (Maybe Text),
    _role :: B.C f Role,
    _gender :: B.C f Gender,
    _identifierType :: B.C f IdentifierType,
    _email :: B.C f (Maybe Text),
    _mobileNumber :: B.C f (Maybe Text),
    _mobileCountryCode :: B.C f (Maybe Text),
    _identifier :: B.C f (Maybe Text),
    _rating :: B.C f (Maybe Text),
    _verified :: B.C f Bool,
    _udf1 :: B.C f (Maybe Text),
    _udf2 :: B.C f (Maybe Text),
    _status :: B.C f Status,
    _organizationId :: B.C f (Maybe Text),
    _locationId :: B.C f (Maybe Text),
    _deviceToken :: B.C f (Maybe FCM.FCMRecipientToken),
    _description :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type Person = PersonT Identity

type PersonPrimaryKey = B.PrimaryKey PersonT Identity

instance B.Table PersonT where
  data PrimaryKey PersonT f = PersonPrimaryKey (B.C f PersonId)
    deriving (Generic, B.Beamable)
  primaryKey = PersonPrimaryKey . _id

deriving instance Show Person

deriving instance Eq Person

instance ToJSON Person where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Person where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Person

insertExpression org = insertExpressions [org]

insertExpressions orgs = B.insertValues orgs

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity PersonT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at",
        _updatedAt = "updated_at",
        _firstName = "first_name",
        _middleName = "middle_name",
        _lastName = "last_name",
        _fullName = "full_name",
        _mobileNumber = "mobile_number",
        _organizationId = "organization_id",
        _locationId = "location_id",
        _mobileCountryCode = "mobile_country_code",
        _identifierType = "identifier_type",
        _deviceToken = "device_token"
      }
