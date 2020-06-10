{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Epass.Types.Storage.Pass where

import qualified Beckn.Types.Storage.Location as BTL
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.MySQL
import Epass.Types.App
import Epass.Types.Common (Bound (..), PassType (..))
import EulerHS.Prelude
import Servant.API
import Servant.Swagger

data Status = ACTIVE | REVOKED | EXPIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Status

instance FromHttpApiData Status where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict

data PassT f = Pass
  { _id :: B.C f PassId,
    _CustomerId :: B.C f (Maybe CustomerId),
    _ShortId :: B.C f Text,
    _OrganizationId :: B.C f (Maybe OrganizationId),
    _TenantOrganizationId :: B.C f (Maybe TenantOrganizationId),
    _status :: B.C f Status,
    _fromDate :: B.C f LocalTime,
    _toDate :: B.C f LocalTime,
    _passType :: B.C f PassType,
    _PassApplicationId :: B.C f PassApplicationId,
    _CreatedBy :: B.C f CustomerId,
    _info :: B.C f Text,
    _fromLocationType :: B.C f (Maybe BTL.LocationType),
    _fromLat :: B.C f (Maybe Double),
    _fromLong :: B.C f (Maybe Double),
    _fromWard :: B.C f (Maybe Text),
    _fromDistrict :: B.C f (Maybe Text),
    _fromCity :: B.C f (Maybe Text),
    _fromState :: B.C f (Maybe Text),
    _fromCountry :: B.C f (Maybe Text),
    _fromPincode :: B.C f (Maybe Int),
    _fromAddress :: B.C f (Maybe Text),
    _fromBound :: B.C f (Maybe Bound),
    _toLocationType :: B.C f (Maybe BTL.LocationType),
    _toLat :: B.C f (Maybe Double),
    _toLong :: B.C f (Maybe Double),
    _toWard :: B.C f (Maybe Text),
    _toDistrict :: B.C f (Maybe Text),
    _toCity :: B.C f (Maybe Text),
    _toState :: B.C f (Maybe Text),
    _toCountry :: B.C f (Maybe Text),
    _toPincode :: B.C f (Maybe Int),
    _toAddress :: B.C f (Maybe Text),
    _toBound :: B.C f (Maybe Bound),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type Pass = PassT Identity

type PassPrimaryKey = B.PrimaryKey PassT Identity

instance B.Table PassT where
  data PrimaryKey PassT f = PassPrimaryKey (B.C f PassId)
    deriving (Generic, B.Beamable)
  primaryKey = PassPrimaryKey . _id

deriving instance Show Pass

deriving instance Eq Pass

instance ToJSON Pass where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Pass where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Pass

insertExpression customer = insertExpressions [customer]

insertExpressions customers = B.insertValues customers

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity PassT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _OrganizationId = "organization_id",
        _TenantOrganizationId = "tenant_organization_id",
        _ShortId = "short_id",
        _CustomerId = "customer_id",
        _fromDate = "from_date",
        _toDate = "to_date",
        _passType = "pass_type",
        _PassApplicationId = "pass_application_id",
        _fromLocationType = "from_location_type",
        _fromLat = "from_lat",
        _fromLong = "from_long",
        _fromWard = "from_ward",
        _fromDistrict = "from_district",
        _fromCity = "from_city",
        _fromState = "from_state",
        _fromCountry = "from_country",
        _fromPincode = "from_pincode",
        _fromAddress = "from_address",
        _fromBound = "from_bound",
        _toLocationType = "to_location_type",
        _toLat = "to_lat",
        _toLong = "to_long",
        _toWard = "to_ward",
        _toDistrict = "to_district",
        _toCity = "to_city",
        _toState = "to_state",
        _toCountry = "to_country",
        _toPincode = "to_pincode",
        _toAddress = "to_address",
        _toBound = "to_bound",
        _CreatedBy = "created_by",
        _createdAt = "created_at",
        _updatedAt = "updated_at"
      }
