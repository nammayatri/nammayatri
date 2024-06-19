{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.RidePayment where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data PaymentIntentResponse = PaymentIntentResponse {customerId :: Kernel.External.Payment.Interface.Types.CustomerId, ephemeralKey :: Kernel.Prelude.Text, paymentIntentClientSecret :: Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PaymentMethodsResponse = PaymentMethodsResponse {list :: Kernel.External.Payment.Interface.Types.CustomerCardListResp} deriving (Generic, ToJSON, FromJSON, ToSchema)

data SetupIntentResponse = SetupIntentResponse {customerId :: Kernel.External.Payment.Interface.Types.CustomerId, ephemeralKey :: Kernel.Prelude.Text, setupIntentClientSecret :: Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)
