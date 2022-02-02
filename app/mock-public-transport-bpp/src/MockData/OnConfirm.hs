module MockData.OnConfirm where

import qualified Core.Confirm as Confirm
import qualified Core.Confirm.Item as Confirm
import Core.Descriptor
import Core.OnConfirm
import Core.OrderState
import Core.Payment
import Data.Either.Extra
--import MockData.OnSearch
import Relude hiding (id, state)
import Servant.Client
import Beckn.Types.Core.Migration.DecimalValue
import Core.Fulfillment
import Core.Location (LocationId(LocationId))
import Core.Time
import Core.Quantity
import Core.OnConfirm.Item
import Core.OnConfirm.Params
import Core.OnConfirm.Order

buildOnConfirmMessage :: Text -> Confirm.Order -> Either Text OnConfirmMessage
buildOnConfirmMessage orderId confOrd = do
  order <- buildOnConfirmOrder orderId confOrd
  pure OnConfirmMessage {..}

buildOnConfirmOrder :: Text -> Confirm.Order -> Either Text Order
buildOnConfirmOrder orderId confOrd = do
  let id = orderId
      state = Active
      provider = confOrd.provider
      billing = confOrd.billing
--  fulfillment <- maybeToEither "failed to find fulfillment" $ findFulfillment reqFulfillment
  item <- maybe (Left "no items found") Right $ listToMaybe confOrd.items
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
      fulfillment = buildOnConfirmFulfillment item
      items = map addQrCode confOrd.items
  pure Order {..}

buildOnConfirmFulfillment :: Confirm.Item -> Fulfillment
buildOnConfirmFulfillment item = do
  let id = item.route_code
      start = FulfillmentLocationTime
        { location = LocationId item.start_stop,
          time = Time "Departure time" item.start_time
        }
      end = FulfillmentLocationTime
        { location = LocationId item.end_stop,
          time = Time "Arrival time" item.end_time
        }
  Fulfillment {..}

addQrCode :: Confirm.Item -> Item
addQrCode Confirm.Item {..} =
  let descriptor = DescriptorCode {code = "<QR code date>"}
      id = route_code
      fulfillment_id = route_code
      quantity = Quantity 1
   in Item {..}

defaultPaymentLink :: BaseUrl
defaultPaymentLink =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "www.payment_url.com",
      baseUrlPort = 80,
      baseUrlPath = ""
    }
