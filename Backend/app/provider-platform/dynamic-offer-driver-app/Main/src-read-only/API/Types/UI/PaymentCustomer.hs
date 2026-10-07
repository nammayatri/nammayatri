{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PaymentCustomer where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data PaymentCustomerResp = PaymentCustomerResp {clientAuthToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text, clientAuthTokenExpiry :: Kernel.Prelude.Maybe Data.Time.UTCTime, customerId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
