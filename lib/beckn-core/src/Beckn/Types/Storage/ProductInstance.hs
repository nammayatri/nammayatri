{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.ProductInstance where

import Beckn.Types.App
import Beckn.Types.Core.Amount
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

data ProductInstanceStatus = VALID | INVALID | INPROGRESS | CONFIRMED | COMPLETED | INSTOCK | OUTOFSTOCK | CANCELLED | EXPIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductInstanceStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres ProductInstanceStatus

instance FromBackendRow Postgres ProductInstanceStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData ProductInstanceStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data EntityType = VEHICLE | PASS | TICKET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres EntityType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductInstanceT f = ProductInstance
  { _id :: B.C f ProductInstanceId,
    _caseId :: B.C f CaseId,
    _productId :: B.C f ProductsId,
    _personId :: B.C f (Maybe PersonId),
    _shortId :: B.C f Text,
    _entityType :: B.C f EntityType,
    _entityId :: B.C f (Maybe Text),
    _quantity :: B.C f Int,
    _price :: B.C f Amount,
    _status :: B.C f ProductInstanceStatus,
    _startTime :: B.C f LocalTime,
    _endTime :: B.C f (Maybe LocalTime),
    _validTill :: B.C f LocalTime,
    _fromLocation :: B.C f (Maybe Text),
    _toLocation :: B.C f (Maybe Text),
    _organizationId :: B.C f Text,
    _parentId :: B.C f (Maybe ProductInstanceId),
    _udf1 :: B.C f (Maybe Text),
    _udf2 :: B.C f (Maybe Text),
    _udf3 :: B.C f (Maybe Text),
    _udf4 :: B.C f (Maybe Text),
    _udf5 :: B.C f (Maybe Text),
    _info :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

--TODO: _organizationId - -- need to point to primarykey

type ProductInstance = ProductInstanceT Identity

type ProductInstancePrimaryKey = B.PrimaryKey ProductInstanceT Identity

instance B.Table ProductInstanceT where
  data PrimaryKey ProductInstanceT f = ProductInstancePrimaryKey (B.C f ProductInstanceId)
    deriving (Generic, B.Beamable)
  primaryKey = ProductInstancePrimaryKey . _id

deriving instance Show ProductInstance

deriving instance Eq ProductInstance

instance ToJSON ProductInstance where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON ProductInstance where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema ProductInstance

insertExpression products = B.insertValues [products]

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ProductInstanceT)
fieldEMod =
  B.setEntityName "product_instance"
    <> B.modifyTableFields
      B.tableModification
        { _caseId = "case_id",
          _productId = "product_id",
          _personId = "person_id",
          _entityType = "entity_type",
          _entityId = "entity_id",
          _startTime = "start_time",
          _endTime = "end_time",
          _shortId = "short_id",
          _validTill = "valid_till",
          _fromLocation = "from_location_id",
          _toLocation = "to_location_id",
          _parentId = "parent_id",
          _organizationId = "organization_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
