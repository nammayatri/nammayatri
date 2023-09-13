{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage.Beam.Invoice where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Invoice as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.Prelude (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

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

$(enableKVPG ''InvoiceT ['id] [])
$(mkTableInstances ''InvoiceT "invoice")
