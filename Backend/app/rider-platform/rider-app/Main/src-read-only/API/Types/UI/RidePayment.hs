{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RidePayment where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth

newtype AddTipRequest = AddTipRequest {amount :: Kernel.Types.Common.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ClearDuesReq = ClearDuesReq {paymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ClearDuesResp = ClearDuesResp
  { amountCleared :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder),
    ridesCleared :: [Kernel.Types.Id.Id Domain.Types.Ride.Ride],
    status :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DueAmountRide = DueAmountRide {amount :: Kernel.Types.Common.HighPrecMoney, rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetDueAmountResp = GetDueAmountResp {currency :: Kernel.Types.Common.Currency, rides :: [DueAmountRide], totalDueAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentIntentResponse = PaymentIntentResponse {customerId :: Kernel.External.Payment.Interface.Types.CustomerId, ephemeralKey :: Kernel.Prelude.Text, paymentIntentClientSecret :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentMethodsResponse = PaymentMethodsResponse {defaultPaymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId, list :: Kernel.External.Payment.Interface.Types.CustomerCardListResp}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SetupIntentResponse = SetupIntentResponse {customerId :: Kernel.External.Payment.Interface.Types.CustomerId, ephemeralKey :: Kernel.Prelude.Text, setupIntentClientSecret :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
