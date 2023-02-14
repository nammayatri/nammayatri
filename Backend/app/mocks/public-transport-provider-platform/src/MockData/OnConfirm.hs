 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module MockData.OnConfirm where

import "public-transport-rider-platform" Beckn.Spec.Common
import qualified "public-transport-rider-platform" Beckn.Spec.Confirm as Confirm
import "public-transport-rider-platform" Beckn.Spec.OnConfirm
import Data.Either.Extra
import Relude hiding (id, state)
import Servant.Client

makeOnConfirmMessage :: Text -> Confirm.Order -> Either Text OnConfirmMessage
makeOnConfirmMessage orderId confOrd = do
  order <- makeOnConfirmOrder orderId confOrd
  pure OnConfirmMessage {..}

makeOnConfirmOrder :: Text -> Confirm.Order -> Either Text Order --why do we need either here?
makeOnConfirmOrder orderId confOrd = do
  let id = orderId
      state = ACTIVE
      provider = confOrd.provider
      billing = confOrd.billing
  let quote = confOrd.quote
      payment =
        Payment
          { uri = defaultPaymentLink,
            tl_method = HttpGet,
            _type = PRE_FULFILLMENT,
            status = NOT_PAID,
            params =
              Params
                { transaction_id = "payment_transaction_id",
                  transaction_status = PAYMENT_LINK_CREATED,
                  amount = realToFrac $ confOrd.payment.params.amount,
                  currency = confOrd.payment.params.currency
                }
          }
      items = map makeOnConfirmItem confOrd.items
  pure Order {..}

makeOnConfirmItem :: Confirm.Item -> Item
makeOnConfirmItem Confirm.Item {..} = do
  let quantity = Quantity 1
  Item {..}

defaultPaymentLink :: BaseUrl
defaultPaymentLink =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "www.payment_url.com",
      baseUrlPort = 80,
      baseUrlPath = ""
    }
