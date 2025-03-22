{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PaymentCustomer where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PaymentCustomerT f = PaymentCustomerT
  { clientAuthToken :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientAuthTokenExpiry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    customerId :: (B.C f Kernel.External.Payment.Interface.Types.CustomerId),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentCustomerT where
  data PrimaryKey PaymentCustomerT f = PaymentCustomerId (B.C f Kernel.External.Payment.Interface.Types.CustomerId) deriving (Generic, B.Beamable)
  primaryKey = PaymentCustomerId . customerId

type PaymentCustomer = PaymentCustomerT Identity

$(enableKVPG (''PaymentCustomerT) [('customerId)] [])

$(mkTableInstances (''PaymentCustomerT) "payment_customer")
