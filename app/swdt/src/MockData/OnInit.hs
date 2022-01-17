module MockData.OnInit where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Core.Migration.DecimalValue
import Beckn.Types.Core.Migration.Duration
import Core.Init.Item
import qualified Core.Init.Order as Init
import Core.OnInit.Order
import Core.OnInit.Payment
import Core.OnInit.Quotation
import Core.Payment hiding (Params, Payment)
import Core.Price
import Data.Either.Extra
import Data.Maybe
import Data.Monoid
import MockData.OnSearch
import Servant.Client

onInitMessage :: Init.Order -> OnInitMessage
onInitMessage ord = OnInitMessage {order = mockOrder ord}

mockOrder :: Init.Order -> Order
mockOrder ord =
  let provider = ord.provider
      items = ord.items
      billing = ord.billing
      fulfillment = mockFulfillmentEMB
      quote =
        Quotation
          { price = priceEMB,
            breakup =
              (: []) $
                BreakupItem
                  { title = "One Way Ticket",
                    price = priceEMB
                  },
            ttl = Duration "duration"
          }
      payment = buildFakePayment priceEMB.currency priceEMB.value
   in Order {..}

buildFakePayment :: Text -> DecimalValue -> Payment
buildFakePayment cur val =
  Payment
    { uri = fromJust $ parseBaseUrl "https://payment.juspay.in/fake",
      tl_method = HttpGet,
      params =
        Params
          { currency = cur,
            amount = val
          },
      _type = PRE_FULFILLMENT,
      status = NOT_PAID
    }

buildOrderWithLogic :: Init.Order -> Either Text Order
buildOrderWithLogic ord = do
  let provider = ord.provider
      billing = ord.billing
      orderItems = ord.items
  fulfillment <- maybeToEither "failed to find fulfillment" $ findFulfillment $ ord.fulfillment.id
  let processedItemList = mapMaybe processItem orderItems
  when (null processedItemList) $ Left "no valid items found"
  let (items, Sum totalPrice, breakups) = foldMap (\(x, y, z) -> ([x], Sum y, z)) processedItemList
      decimalTotalPrice = convertAmountToDecimalValue totalPrice
      quote =
        Quotation
          { price = Price "INR" decimalTotalPrice,
            breakup = breakups,
            ttl = Duration "duration"
          }
      payment = buildFakePayment "INR" decimalTotalPrice
  pure Order {..}

processItem :: Item -> Maybe (Item, Amount, [BreakupItem])
processItem it = do
  existingItem <- findItem it.id it.fulfillment_id
  let quan = it.quantity.count :: Int
  let oneItemPrice = existingItem.price
  oneItemAmount <- convertDecimalValueToAmount oneItemPrice.value
  let total = oneItemAmount * fromIntegral quan
      title = existingItem.descriptor.name
      breakupItems = replicate quan $ BreakupItem title oneItemPrice
  pure (it, total, breakupItems)
