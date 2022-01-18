module MockData.OnConfirm where

import Beckn.Prelude
import qualified Core1.Confirm as Confirm
import Core1.Descriptor
import Core1.Item
import Core1.OnConfirm
import Core1.Payment
import Data.Either.Extra
import MockData.OnSearch
import Servant.Client

buildOnConfirmMessage :: Confirm.Order -> Either Text OnConfirmMessage
buildOnConfirmMessage confOrd = do
  order <- buildOnConfirmOrder confOrd
  pure OnConfirmMessage {..}

buildOnConfirmOrder :: Confirm.Order -> Either Text Order
buildOnConfirmOrder confOrd = do
  let id = "ORDER_1"
      state = Active
      provider = confOrd.provider
      billing = confOrd.billing
      reqFulfillment = confOrd.fulfillment.id
  fulfillment <- maybeToEither "failed to find fulfillment" $ findFulfillment reqFulfillment
  let quote = confOrd.quote -- should check if everything is correct
      payment =
        Payment
          { uri = defaultPaymentLink,
            tl_method = HttpGet,
            _type = PRE_FULFILLMENT,
            status = NOT_PAID,
            params =
              OnConfirmParams
                { transaction_id = "payment_transaction_id",
                  transaction_status = Payment_link_created,
                  amount = confOrd.payment.params.amount,
                  currency = confOrd.payment.params.currency
                }
          }
      items = map addQrCode confOrd.items
  pure Order {..}

addQrCode :: ConfirmItem -> OnConfirmItem
addQrCode InitItem {..} =
  let descriptor = DescriptorCode {code = "<QR code date>"}
   in OnConfirmItem {..}

defaultPaymentLink :: BaseUrl
defaultPaymentLink =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "www.payment_url.com",
      baseUrlPort = 80,
      baseUrlPath = ""
    }
