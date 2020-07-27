{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Inventory where

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

data InventoryStatus = INSTOCK | OUTOFSTOCK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be InventoryStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance B.HasSqlEqualityCheck Postgres InventoryStatus

instance FromBackendRow Postgres InventoryStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromHttpApiData InventoryStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data InventoryT f = Inventory
  { _id :: B.C f InventoryId,
    _productId :: B.C f ProductsId,
    _organizationId :: B.C f OrganizationId,
    _status :: B.C f InventoryStatus,
    _quantity :: B.C f Int,
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type Inventory = InventoryT Identity

type InventoryPrimaryKey = B.PrimaryKey InventoryT Identity

instance B.Table InventoryT where
  data PrimaryKey InventoryT f = InventoryPrimaryKey (B.C f InventoryId)
    deriving (Generic, B.Beamable)
  primaryKey = InventoryPrimaryKey . _id

deriving instance Show Inventory

deriving instance Eq Inventory

instance ToJSON Inventory where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Inventory where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Inventory

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity InventoryT)
fieldEMod =
  B.setEntityName "inventory"
    <> B.modifyTableFields
      B.tableModification
        { _productId = "product_id",
          _organizationId = "organization_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
