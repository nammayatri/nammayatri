module MockData.OnConfirm where

import Beckn.Types.Core.Migration.DecimalValue
import Core.Common.OrderState
import Core.Common.Payment
import qualified Core.Confirm as Confirm
import qualified Core.Confirm.Item as Confirm
import Core.OnConfirm
import Core.OnConfirm.Item
import Core.OnConfirm.Order
import Core.OnConfirm.Params
import Core.OnConfirm.Quantity
import Data.Either.Extra
import Relude hiding (id, state)
import Servant.Client

makeOnConfirmMessage :: Text -> Confirm.Order -> Either Text OnConfirmMessage
makeOnConfirmMessage orderId confOrd = do
  order <- makeOnConfirmOrder orderId confOrd
  pure OnConfirmMessage {..}

makeOnConfirmOrder :: Text -> Confirm.Order -> Either Text Order
makeOnConfirmOrder orderId confOrd = do
  let id = orderId
      state = Active
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
                  transaction_status = PaymentLinkCreated,
                  amount = convertAmountToDecimalValue $ confOrd.payment.params.amount,
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
