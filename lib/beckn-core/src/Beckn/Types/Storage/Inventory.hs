{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Inventory where

import Beckn.Types.Id
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Types.Storage.Products (Products)
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
  { id :: B.C f (Id Inventory),
    productId :: B.C f (Id Products),
    organizationId :: B.C f (Id Organization),
    status :: B.C f InventoryStatus,
    quantity :: B.C f Int,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Inventory = InventoryT Identity

type InventoryPrimaryKey = B.PrimaryKey InventoryT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table InventoryT where
  data PrimaryKey InventoryT f = InventoryPrimaryKey (B.C f (Id Inventory))
    deriving (Generic, B.Beamable)
  primaryKey = InventoryPrimaryKey . id

deriving instance Show Inventory

deriving instance Eq Inventory

instance ToJSON Inventory where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Inventory where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Inventory

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity InventoryT)
fieldEMod =
  B.setEntityName "inventory"
    <> B.modifyTableFields
      B.tableModification
        { productId = "product_id",
          organizationId = "organization_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
