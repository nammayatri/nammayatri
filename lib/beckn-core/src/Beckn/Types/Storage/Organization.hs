{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Organization where

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

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Status

instance FromHttpApiData Status where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

--------------------------------------------------------------------------------------

data OrganizationType
  = PROVIDER
  | APP
  | GATEWAY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrganizationType where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres OrganizationType

instance FromBackendRow Postgres OrganizationType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema OrganizationType

instance FromHttpApiData OrganizationType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data OrganizationDomain
  = MOBILITY
  | FINAL_MILE_DELIVERY
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  deriving (Show, Eq, Read, Generic, ToSchema)

orgDomainOptions :: Options
orgDomainOptions =
  defaultOptions
    { constructorTagModifier = T.unpack . T.replace "_" "-" . T.pack
    }

instance ToJSON OrganizationDomain where
  toJSON = genericToJSON orgDomainOptions

instance FromJSON OrganizationDomain where
  parseJSON = genericParseJSON orgDomainOptions

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrganizationDomain where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres OrganizationDomain

instance FromBackendRow Postgres OrganizationDomain where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema OrganizationDomain

instance FromHttpApiData OrganizationDomain where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data OrganizationT f = Organization
  { _id :: B.C f OrganizationId,
    _name :: B.C f Text,
    _description :: B.C f (Maybe Text),
    _mobileNumber :: B.C f (Maybe Text),
    _mobileCountryCode :: B.C f (Maybe Text),
    _gstin :: B.C f (Maybe Text),
    _type :: B.C f OrganizationType,
    _domain :: B.C f (Maybe OrganizationDomain),
    _locationId :: B.C f (Maybe Text),
    _fromTime :: B.C f (Maybe LocalTime),
    _toTime :: B.C f (Maybe LocalTime),
    _headCount :: B.C f (Maybe Int),
    _status :: B.C f Status,
    _verified :: B.C f Bool,
    _enabled :: B.C f Bool,
    _apiKey :: B.C f (Maybe Text),
    _callbackUrl :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime,
    _callbackApiKey :: B.C f (Maybe Text),
    _info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type Organization = OrganizationT Identity

type OrganizationPrimaryKey = B.PrimaryKey OrganizationT Identity

instance B.Table OrganizationT where
  data PrimaryKey OrganizationT f = OrganizationPrimaryKey (B.C f OrganizationId)
    deriving (Generic, B.Beamable)
  primaryKey = OrganizationPrimaryKey . _id

deriving instance Show Organization

deriving instance Eq Organization

instance ToJSON Organization where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Organization where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Organization

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OrganizationT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at",
        _updatedAt = "updated_at",
        _locationId = "location_id",
        _mobileNumber = "mobile_number",
        _mobileCountryCode = "mobile_country_code",
        _headCount = "head_count",
        _apiKey = "api_key",
        _fromTime = "from_time",
        _toTime = "to_time",
        _callbackUrl = "callback_url",
        _callbackApiKey = "callback_api_key"
      }
