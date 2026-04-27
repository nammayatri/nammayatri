{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.WalletPayments where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.Wallet
import qualified Tools.Beam.UtilsTH

data WalletPayments = WalletPayments
  { campaignId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletPayments.WalletPayments,
    kind :: Lib.Payment.Domain.Types.WalletPayments.WalletPaymentKind,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    personId :: Kernel.Prelude.Text,
    points :: Kernel.Types.Common.HighPrecMoney,
    programId :: Kernel.Prelude.Text,
    status :: Lib.Payment.Domain.Types.WalletPayments.WalletPaymentStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    walletId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data WalletPaymentKind = TOPUP | CASHBACK deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data WalletPaymentStatus = NEW | PENDING | CHARGED | FAILED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''Lib.Payment.Domain.Types.WalletPayments.WalletPaymentKind))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''Lib.Payment.Domain.Types.WalletPayments.WalletPaymentStatus))
