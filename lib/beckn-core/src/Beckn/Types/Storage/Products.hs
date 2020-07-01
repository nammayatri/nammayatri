{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Products where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Scientific
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

data ProdInfo = ProdInfo
  { driverInfo :: Text,
    vehicleInfo :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data ProductsType = RIDE | PASS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ProductsType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductsStatus = INSTOCK | OUTOFSTOCK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres ProductsStatus

instance FromBackendRow Postgres ProductsStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData ProductsStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

type ProductsIndustry = Case.Industry

-- data ProductsIndustry = MOBILITY | GOVT | GROCERY
--   deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsIndustry where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance FromBackendRow Postgres ProductsIndustry where
--   fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductsT f = Products
  { _id :: B.C f ProductsId,
    _shortId :: B.C f Text,
    _name :: B.C f (Maybe Text),
    _description :: B.C f (Maybe Text),
    _industry :: B.C f ProductsIndustry,
    _type :: B.C f ProductsType,
    _status :: B.C f ProductsStatus,
    _startTime :: B.C f LocalTime,
    _endTime :: B.C f (Maybe LocalTime),
    _validTill :: B.C f LocalTime,
    _price :: B.C f Scientific,
    _rating :: B.C f (Maybe Text),
    _review :: B.C f (Maybe Text),
    _udf1 :: B.C f (Maybe Text),
    _udf2 :: B.C f (Maybe Text),
    _udf3 :: B.C f (Maybe Text),
    _udf4 :: B.C f (Maybe Text),
    _udf5 :: B.C f (Maybe Text),
    _info :: B.C f (Maybe Text),
    _fromLocation :: B.C f (Maybe Text),
    _toLocation :: B.C f (Maybe Text),
    _organizationId :: B.C f Text,
    _assignedTo :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

--TODO: _organizationId - -- need to point to primarykey
-- fields => mobility  => pass
-- udf1 => vehicle variant  =>
-- udf2 => luggage_count =>
-- udf3 => vehicle id  =>

type Products = ProductsT Identity

type ProductsPrimaryKey = B.PrimaryKey ProductsT Identity

instance B.Table ProductsT where
  data PrimaryKey ProductsT f = ProductsPrimaryKey (B.C f ProductsId)
    deriving (Generic, B.Beamable)
  primaryKey = ProductsPrimaryKey . _id

deriving instance Show Products

deriving instance Eq Products

instance ToJSON Products where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Products where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Products

insertExpression products = insertExpressions [products]

insertExpressions products = B.insertValues products

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ProductsT)
fieldEMod =
  B.setEntityName "product"
    <> B.modifyTableFields
      B.tableModification
        { _startTime = "start_time",
          _endTime = "end_time",
          _shortId = "short_id",
          _validTill = "valid_till",
          _fromLocation = "from_location_id",
          _toLocation = "to_location_id",
          _organizationId = "organization_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at",
          _assignedTo = "assigned_to"
        }
