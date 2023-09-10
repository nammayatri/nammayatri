{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Invoice where

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
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.InvoiceStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.InvoiceStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.InvoiceStatus

instance FromBackendRow Postgres Domain.InvoiceStatus

instance IsString Domain.InvoiceStatus where
  fromString = show

instance FromField Domain.InvoicePaymentMode where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.InvoicePaymentMode where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.InvoicePaymentMode

instance FromBackendRow Postgres Domain.InvoicePaymentMode

instance IsString Domain.InvoicePaymentMode where
  fromString = show

data InvoiceT f = InvoiceT
  { id :: B.C f Text,
    invoiceShortId :: B.C f Text,
    driverFeeId :: B.C f Text,
    invoiceStatus :: B.C f Domain.InvoiceStatus,
    paymentMode :: B.C f Domain.InvoicePaymentMode,
    maxMandateAmount :: B.C f (Maybe HighPrecMoney),
    bankErrorMessage :: B.C f (Maybe Text),
    bankErrorCode :: B.C f (Maybe Text),
    bankErrorUpdatedAt :: B.C f (Maybe UTCTime),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table InvoiceT where
  data PrimaryKey InvoiceT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Invoice = InvoiceT Identity

invoiceTMod :: InvoiceT (B.FieldModification (B.TableField InvoiceT))
invoiceTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      invoiceShortId = B.fieldNamed "invoice_short_id",
      driverFeeId = B.fieldNamed "driver_fee_id",
      invoiceStatus = B.fieldNamed "invoice_status",
      paymentMode = B.fieldNamed "payment_mode",
      maxMandateAmount = B.fieldNamed "max_mandate_amount",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''InvoiceT ['id] [])
$(mkTableInstances ''InvoiceT "invoice" "atlas_driver_offer_bpp")
