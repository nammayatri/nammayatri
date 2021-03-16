{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Products where

import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Database.Beam as B
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import EulerHS.Prelude
import Servant.API

data ProductsType = RIDE | PASS
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ProductsType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

type ProductsIndustry = Case.Industry

-- data ProductsIndustry = MOBILITY | GOVT | GROCERY
--   deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be ProductsIndustry where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance FromBackendRow Postgres ProductsIndustry where
--   fromBackendRow = read . T.unpack <$> fromBackendRow

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

data ProductsT f = Products
  { _id :: B.C f (Id Products),
    _name :: B.C f Text,
    _description :: B.C f (Maybe Text),
    _industry :: B.C f ProductsIndustry,
    _type :: B.C f ProductsType,
    _shortId :: B.C f Text,
    _status :: B.C f ProductsStatus,
    _price :: B.C f Amount,
    _rating :: B.C f (Maybe Text),
    _review :: B.C f (Maybe Text),
    _info :: B.C f (Maybe Text),
    _udf1 :: B.C f (Maybe Text),
    _udf2 :: B.C f (Maybe Text),
    _udf3 :: B.C f (Maybe Text),
    _udf4 :: B.C f (Maybe Text),
    _udf5 :: B.C f (Maybe Text),
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Products = ProductsT Identity

type ProductsPrimaryKey = B.PrimaryKey ProductsT Identity

instance B.Table ProductsT where
  data PrimaryKey ProductsT f = ProductsPrimaryKey (B.C f (Id Products))
    deriving (Generic, B.Beamable)
  primaryKey = ProductsPrimaryKey . _id

deriving instance Show Products

deriving instance Eq Products

instance ToJSON Products where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Products where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Products

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ProductsT)
fieldEMod =
  B.setEntityName "product"
    <> B.modifyTableFields
      B.tableModification
        { _shortId = "short_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }

validateStatusTransition :: ProductsStatus -> ProductsStatus -> Either Text ()
validateStatusTransition oldState newState =
  if oldState == newState
    then allowed
    else t oldState newState
  where
    forbidden =
      Left $
        T.pack $
          "It is not allowed to change Product status from "
            <> show oldState
            <> " to "
            <> show newState
    allowed = Right ()
    t INSTOCK OUTOFSTOCK = allowed
    t INSTOCK _ = forbidden
    t OUTOFSTOCK INSTOCK = allowed
    t OUTOFSTOCK _ = forbidden
