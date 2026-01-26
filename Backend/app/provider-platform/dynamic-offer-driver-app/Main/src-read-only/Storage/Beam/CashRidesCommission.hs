{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CashRidesCommission where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Person
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Stripe.Types.Transfer
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data CashRidesCommissionT f = CashRidesCommissionT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f Kernel.Types.Common.Currency,
    id :: B.C f Kernel.Prelude.Text,
    lastSettlementTime :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    nextSettlementTime :: B.C f Kernel.Prelude.UTCTime,
    numberOfRides :: B.C f Kernel.Prelude.Int,
    paymentMode :: B.C f Domain.Types.Extra.MerchantPaymentMethod.PaymentMode,
    personId :: B.C f Kernel.Prelude.Text,
    personRole :: B.C f Domain.Types.Person.Role,
    status :: B.C f Kernel.External.Payment.Stripe.Types.Transfer.TransferStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CashRidesCommissionT where
  data PrimaryKey CashRidesCommissionT f = CashRidesCommissionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CashRidesCommissionId . id

type CashRidesCommission = CashRidesCommissionT Identity

$(enableKVPG ''CashRidesCommissionT ['id] [['personId]])

$(mkTableInstances ''CashRidesCommissionT "cash_rides_commission")
