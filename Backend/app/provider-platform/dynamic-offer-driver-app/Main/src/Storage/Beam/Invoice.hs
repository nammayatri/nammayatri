{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage.Beam.Invoice where

import qualified Database.Beam as B
import qualified Domain.Types.Invoice as Domain
import Tools.Beam.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)

data InvoiceT f = InvoiceT
  { id :: B.C f Text,
    invoiceShortId :: B.C f Text,
    driverFeeId :: B.C f Text,
    driverId :: B.C f Text,
    invoiceStatus :: B.C f Domain.InvoiceStatus,
    paymentMode :: B.C f Domain.InvoicePaymentMode,
    maxMandateAmount :: B.C f (Maybe HighPrecMoney),
    lastStatusCheckedAt :: B.C f (Maybe UTCTime),
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
