module MockData.OnConfirm where

import Beckn.Types.Core.Migration.DecimalValue
import "public-transport-bap" Core.Spec.Common
import qualified "public-transport-bap" Core.Spec.Confirm as Confirm
import "public-transport-bap" Core.Spec.OnConfirm
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
