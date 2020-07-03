{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.ProductInstance where

import Beckn.Types.App
import Beckn.Types.Storage.Products (ProductsStatus)
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

data ProductInstanceT f = ProductInstance
  { _id :: B.C f ProductInstanceId,
    _caseId :: B.C f CaseId,
    _productId :: B.C f ProductsId,
    _personId :: B.C f (Maybe PersonId),
    _quantity :: B.C f Int,
    _price :: B.C f Scientific,
    _status :: B.C f ProductInstanceStatus,
    _info :: B.C f (Maybe Text),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

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

insertExpression products = insertExpressions [products]

insertExpressions products = B.insertValues products

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ProductInstanceT)
fieldEMod =
  B.setEntityName "product_instance"
    <> B.modifyTableFields
      B.tableModification
        { _caseId = "case_id",
          _productId = "product_id",
          _personId = "person_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
