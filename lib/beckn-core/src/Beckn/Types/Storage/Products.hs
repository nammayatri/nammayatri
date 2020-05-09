{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Products where

import Beckn.Types.App
import Data.Swagger
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.MySQL
import EulerHS.Prelude --(FromJSON, ToJSON, toJSON, parseJSON, Eq, Maybe)
import Servant.Swagger

data ProductsType = RIDE | PASS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL ProductsType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductsStatus = VALID | INPROGRESS | INSTOCK | OUTOFSTOCK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL ProductsStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductsIndustry = MOBILITY | GOVT | GROCERY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsIndustry where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL ProductsIndustry where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ProductsT f = Products
  { _id :: B.C f ProductsId,
    _name :: B.C f (Maybe Text),
    _description :: B.C f (Maybe Text),
    _industry :: B.C f ProductsIndustry,
    _type :: B.C f ProductsType,
    _status :: B.C f ProductsStatus,
    _startTime :: B.C f LocalTime,
    _endTime :: B.C f (Maybe LocalTime),
    _validTill :: B.C f LocalTime,
    _price :: B.C f Double,
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
  B.modifyTableFields
    B.tableModification
      { _startTime = "start_time",
        _endTime = "end_time",
        _validTill = "valid_till",
        _fromLocation = "from_location",
        _toLocation = "to_location",
        _organizationId = "organization_id",
        _createdAt = "created_at",
        _updatedAt = "updated_at"
      }
