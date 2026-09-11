{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.BookingDeposit where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data BookingDepositPaymentResp = BookingDepositPaymentResp
  { availableBalance :: Kernel.Types.Common.HighPrecMoney,
    feeStatus :: BookingDepositStatus,
    requiredAmount :: Kernel.Types.Common.HighPrecMoney,
    sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.CreateOrderResp
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingDepositStatus
  = COVERED
  | PAYABLE
  | PROCESSING
  | RETRY
  | FAILED
  | REFUND_IN_PROGRESS
  | REFUND_FAILED
  | REFUNDED
  | FORFEITED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingDepositStatusResp = BookingDepositStatusResp {availableBalance :: Kernel.Types.Common.HighPrecMoney, feeStatus :: BookingDepositStatus, requiredAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
