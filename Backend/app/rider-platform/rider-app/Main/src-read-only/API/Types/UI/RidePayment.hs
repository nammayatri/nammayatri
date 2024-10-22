{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RidePayment where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

newtype AddTipRequest = AddTipRequest {amount :: Kernel.Types.Common.PriceAPIEntity}
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
