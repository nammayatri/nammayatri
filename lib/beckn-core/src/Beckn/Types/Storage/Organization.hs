{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Organization where

import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Servant.API

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres Status

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
    { constructorTagModifier = replaceUnderscoresString
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
  { id :: B.C f (Id Organization),
    name :: B.C f Text,
    description :: B.C f (Maybe Text),
    shortId :: B.C f (ShortId Organization),
    mobileNumber :: B.C f (Maybe Text),
    mobileCountryCode :: B.C f (Maybe Text),
    gstin :: B.C f (Maybe Text),
    _type :: B.C f OrganizationType,
    domain :: B.C f (Maybe OrganizationDomain),
    locationId :: B.C f (Maybe Text),
    fromTime :: B.C f (Maybe UTCTime),
    toTime :: B.C f (Maybe UTCTime),
    headCount :: B.C f (Maybe Int),
    status :: B.C f Status,
    verified :: B.C f Bool,
    enabled :: B.C f Bool,
    apiKey :: B.C f (Maybe Text),
    callbackUrl :: B.C f (Maybe BaseUrl),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    callbackApiKey :: B.C f (Maybe Text),
    info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type Organization = OrganizationT Identity

type OrganizationPrimaryKey = B.PrimaryKey OrganizationT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table OrganizationT where
  data PrimaryKey OrganizationT f = OrganizationPrimaryKey (B.C f (Id Organization))
    deriving (Generic, B.Beamable)
  primaryKey = OrganizationPrimaryKey . id

deriving instance Show Organization

deriving instance Eq Organization

instance ToJSON Organization where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Organization where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Organization

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OrganizationT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { shortId = "short_id",
        createdAt = "created_at",
        updatedAt = "updated_at",
        locationId = "location_id",
        mobileNumber = "mobile_number",
        mobileCountryCode = "mobile_country_code",
        headCount = "head_count",
        apiKey = "api_key",
        fromTime = "from_time",
        toTime = "to_time",
        callbackUrl = "callback_url",
        callbackApiKey = "callback_api_key"
      }
