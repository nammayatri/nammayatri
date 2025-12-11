{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverBankAccount where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.MerchantPaymentMethod
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Stripe.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverBankAccountT f = DriverBankAccountT
  { accountId :: B.C f Kernel.External.Payment.Stripe.Types.AccountId,
    chargesEnabled :: B.C f Kernel.Prelude.Bool,
    currentAccountLink :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currentAccountLinkExpiry :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    detailsSubmitted :: B.C f Kernel.Prelude.Bool,
    driverId :: B.C f Kernel.Prelude.Text,
    paymentMode :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverBankAccountT where
  data PrimaryKey DriverBankAccountT f = DriverBankAccountId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverBankAccountId . driverId

type DriverBankAccount = DriverBankAccountT Identity

$(enableKVPG ''DriverBankAccountT ['driverId] [['accountId]])

$(mkTableInstances ''DriverBankAccountT "driver_bank_account")
