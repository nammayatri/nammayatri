{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage.Beam.Invoice where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Invoice as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.Prelude (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

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

type Invoice = InvoiceT Identity

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

$(enableKVPG ''InvoiceT ['id] [])
$(mkTableInstances ''InvoiceT "invoice" "atlas_driver_offer_bpp")
