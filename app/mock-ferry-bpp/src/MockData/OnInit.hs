module MockData.OnInit where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Core.Migration.DecimalValue
import Beckn.Types.Core.Migration.Duration
import qualified Core1.Init as Init
import Core1.Item
import Core1.OnInit
import Core1.Payment
import Core1.Quotation
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
        OnInitQuotation
          { price = priceEMB,
            breakup =
              (: []) $
                BreakupItem
                  { title = "One Way Ticket",
                    price = priceEMB
                  },
            ttl = Just $ Duration "duration"
          }
      payment = buildFakePayment priceEMB.currency priceEMB.value
   in Order {..}

buildFakePayment :: Text -> DecimalValue -> OnInitPayment
buildFakePayment cur val =
  Payment
    { uri = fromJust $ parseBaseUrl "https://payment.juspay.in/fake",
      tl_method = HttpGet,
      params =
        OnInitParams
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
      reqFulfillment = ord.fulfillment.id
  fulfillment <- maybeToEither "failed to find fulfillment" $ findFulfillment reqFulfillment
  let processedItemList = mapMaybe (processItem reqFulfillment) orderItems
  when (null processedItemList) $ Left "no valid items found"
  let (items, Sum totalPrice, breakups) = foldMap (\(x, y, z) -> ([x], Sum y, z)) processedItemList
      decimalTotalPrice = convertAmountToDecimalValue totalPrice
      quote =
        OnInitQuotation
          { price = buildPriceDecimal decimalTotalPrice,
            breakup = breakups,
            ttl = Just $ Duration "duration"
          }
      payment = buildFakePayment "INR" decimalTotalPrice
  pure Order {..}

processItem :: Text -> InitItem -> Maybe (InitItem, Amount, [BreakupItem])
processItem reqFulfId it = do
  existingItem <- findItem it.id it.fulfillment_id
  guard (it.fulfillment_id == reqFulfId)
  let quan = it.quantity.count :: Int
      oneItemPrice = existingItem.price
  oneItemAmount <- convertDecimalValueToAmount oneItemPrice.value
  let total = oneItemAmount * fromIntegral quan
      title = existingItem.descriptor.name
      breakupItems = replicate quan $ BreakupItem title oneItemPrice
  pure (it, total, breakupItems)
