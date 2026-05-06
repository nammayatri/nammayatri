{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.WalletPayments where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data WalletPayments = WalletPayments
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    domainEntityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletPayments.WalletPayments,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    personId :: Kernel.Prelude.Text,
    status :: Lib.Payment.Domain.Types.WalletPayments.WalletPaymentStatus,
    totalBurned :: Kernel.Types.Common.HighPrecMoney,
    totalEarned :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data WalletPaymentKind = TOPUP | CASHBACK | BURN deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WalletPaymentStatus = NEW | PENDING | CHARGED | FAILED | REVERSED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''Lib.Payment.Domain.Types.WalletPayments.WalletPaymentKind)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''Lib.Payment.Domain.Types.WalletPayments.WalletPaymentStatus)
