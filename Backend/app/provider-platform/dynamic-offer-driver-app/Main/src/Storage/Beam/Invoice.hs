{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Invoice where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Invoice as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.Prelude (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.InvoiceStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.InvoiceStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.InvoiceStatus

instance FromBackendRow Postgres Domain.InvoiceStatus

instance IsString Domain.InvoiceStatus where
  fromString = show

data InvoiceT f = InvoiceT
  { id :: B.C f Text,
    invoiceShortId :: B.C f Text,
    driverFeeId :: B.C f Text,
    invoiceStatus :: B.C f Domain.InvoiceStatus,
    maxMandateAmount :: B.C f (Maybe HighPrecMoney),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table InvoiceT where
  data PrimaryKey InvoiceT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta InvoiceT where
  modelFieldModification = invoiceTMod
  modelTableName = "invoice"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Invoice = InvoiceT Identity

instance FromJSON Invoice where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Invoice where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Invoice

invoiceTMod :: InvoiceT (B.FieldModification (B.TableField InvoiceT))
invoiceTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      invoiceShortId = B.fieldNamed "invoice_short_id",
      driverFeeId = B.fieldNamed "driver_fee_id",
      invoiceStatus = B.fieldNamed "invoice_status",
      maxMandateAmount = B.fieldNamed "max_mandate_amount",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize Invoice where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

invoiceToHSModifiers :: M.Map Text (A.Value -> A.Value)
invoiceToHSModifiers =
  M.empty

invoiceToPSModifiers :: M.Map Text (A.Value -> A.Value)
invoiceToPSModifiers =
  M.empty

$(enableKVPG ''InvoiceT ['id] [])
