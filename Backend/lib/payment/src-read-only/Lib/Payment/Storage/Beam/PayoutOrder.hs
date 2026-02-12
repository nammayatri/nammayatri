{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PayoutOrder where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Common

data PayoutOrderT f = PayoutOrderT
  { accountDetailsType :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Payout.Juspay.Types.Payout.AccountDetailsType)),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    price :: (B.C f Kernel.Types.Common.HighPrecMoney),
    city :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    customerEmailEncrypted :: (B.C f Kernel.Prelude.Text),
    customerEmailHash :: (B.C f Kernel.External.Encryption.DbHash),
    customerId :: (B.C f Kernel.Prelude.Text),
    entityIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    entityName :: (B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName)),
    id :: (B.C f Kernel.Prelude.Text),
    lastStatusCheckedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    mobileNoEncrypted :: (B.C f Kernel.Prelude.Text),
    mobileNoHash :: (B.C f Kernel.External.Encryption.DbHash),
    orderId :: (B.C f Kernel.Prelude.Text),
    responseCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    responseMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    retriedOrderId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    shortId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    status :: (B.C f Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vpa :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutOrderT where
  data PrimaryKey PayoutOrderT f = PayoutOrderId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutOrderId <$> id <*> orderId

type PayoutOrder = PayoutOrderT Identity

$(enableKVPG (''PayoutOrderT) [('id), ('orderId)] [])

$(mkTableInstancesGenericSchema (''PayoutOrderT) "payout_order")
