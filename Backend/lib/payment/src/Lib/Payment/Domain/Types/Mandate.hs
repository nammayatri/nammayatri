{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.Domain.Types.Mandate where

import Kernel.Prelude

data MandateRequest = MandateRequest
  { orderId :: Text,
    merchantId :: Text,
    paymentMethodType :: Text,
    paymentMethod :: Text,
    upiVPA :: Text,
    redirectAfterPayment :: Bool,
    format :: Text,
    mandateType :: Text,
    should_create_mandate :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)

data MandateResponse = MandateResponse
  { status :: Text,
    paymentAuthenticationMethod :: Maybe Text,
    paymentAuthenticationUrl :: Maybe Text,
    paymentAuthenticationParams :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, ToSchema)
