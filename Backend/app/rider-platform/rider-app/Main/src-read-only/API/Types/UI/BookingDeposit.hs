{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.BookingDeposit where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Booking
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data BookingDepositPaymentIntentReq = BookingDepositPaymentIntentReq {bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingDepositPaymentIntentResp = BookingDepositPaymentIntentResp
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
