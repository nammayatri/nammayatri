{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PaymentCustomer where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.MerchantServiceConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PaymentCustomerT f = PaymentCustomerT
  { clientAuthToken :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientAuthTokenExpiry :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    customerId :: (B.C f Kernel.Prelude.Text),
    driverId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    paymentServiceName :: (B.C f Domain.Types.MerchantServiceConfig.ServiceName),
    serviceName :: (B.C f Domain.Types.Extra.Plan.ServiceNames),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentCustomerT where
  data PrimaryKey PaymentCustomerT f = PaymentCustomerId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.Extra.Plan.ServiceNames) deriving (Generic, B.Beamable)
  primaryKey = PaymentCustomerId <$> driverId <*> serviceName

type PaymentCustomer = PaymentCustomerT Identity

$(enableKVPG (''PaymentCustomerT) [('driverId), ('serviceName)] [])

$(mkTableInstances (''PaymentCustomerT) "payment_customer")
