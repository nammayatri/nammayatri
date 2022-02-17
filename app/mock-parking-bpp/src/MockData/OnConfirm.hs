module MockData.OnConfirm where

import Beckn.Mock.Utils
import Beckn.Types.Amount
import Beckn.Types.Core.Migration.DecimalValue
import qualified Core.Common.Payment as Common
import qualified Core.Confirm as Confirm
import Core.OnConfirm
import qualified Core.OnConfirm.Order as OnConfirm
import qualified Core.OnSearch.Item as OnSearch
import MockData.OnSearch as OnSearch
import Relude hiding (id, state)
import Servant.Client
import Utils

toOnConfirmProvider :: Location -> Provider
toOnConfirmProvider loc = do
  let id = mockProviderId
      descriptor = mockProviderDescriptor
      locations = [loc]
  Provider {..}

buildOnConfirmOrder :: Text -> Confirm.Order -> Either Text OnConfirm.Order
buildOnConfirmOrder orderId confOrd = do
  let id = orderId
  oneItem <- validateUniqueItem confOrd.items
  existingItem <- maybeToEither "" $ find (\it -> it.id == oneItem.id) mockItems
  let locationId = existingItem.location_id
  location <-
    maybeToEither ("Internal error: failed to find the location " <> locationId) $
      find (\loc -> loc.id == locationId) OnSearch.allLocations

  itemPriceValue <- maybeToEither "invalid item price" $ convertDecimalValueToAmount existingItem.price.value
  let provider = toOnConfirmProvider location
      billing = confOrd.billing
      state = Just ACTIVE
      provider_location = OrderProviderLocation locationId
      quantity = oneItem.quantity
      item = toOnConfirmItem quantity.count existingItem
      items = [item]
      fulfillment = toOnConfirmFulfillment location confOrd.fulfillment
      quote = toOnConfirmQuote quantity.count itemPriceValue
      totalPriceValue = itemPriceValue * fromIntegral quantity.count
      payment = buildPayment totalPriceValue
  pure OnConfirm.Order {..}

validateUniqueLocationId :: [Location] -> Either Text Text
validateUniqueLocationId = fmap (.id) . validateUnique "location"

validateUniqueItem :: [Confirm.Item] -> Either Text Confirm.Item
validateUniqueItem = validateUnique "item"

toOnConfirmItem :: Int -> OnSearch.Item -> Item
toOnConfirmItem quan onSearchItem = do
  let id = onSearchItem.id
      price = onSearchItem.price
      quantity = Quantity quan
  Item {..}

toOnConfirmFulfillment :: Location -> Confirm.Fulfillment -> Fulfillment
toOnConfirmFulfillment loc confFulf = do
  let _type = "store-pickup"
      tracking = False
      start =
        Start
          { contact = parkingContact,
            time = confFulf.start.time,
            location =
              StartLocation
                { id = loc.id,
                  descriptor = mockProviderDescriptor,
                  gps = loc.gps
                }
          }
      end =
        End
          { time = confFulf.end.time
          }
      vehicle = confFulf.vehicle
  Fulfillment {..}

toOnConfirmQuote :: Int -> Amount -> SpecQuote
toOnConfirmQuote quan amount = do
  let totalPriceValue = amount * fromIntegral quan
      price = buildPriceAmount totalPriceValue
      breakupItem =
        Breakup
          { title = "Four wheeler parking",
            price = buildPriceAmount amount
          }
      breakup = replicate quan breakupItem
  SpecQuote {..}

unDecimalValue :: DecimalValue -> Text
unDecimalValue (DecimalValue x) = x

buildPayment :: Amount -> Common.Payment
buildPayment totalAmount = do
  let _type = Common.PRE_FULFILLMENT
      status = Common.NOT_PAID
      tl_method = "http/get"
      uri = BaseUrl Https "rzp.io" 80 "i/Ag4Op0s"
      totalPrice = unDecimalValue $ convertAmountToDecimalValue totalAmount
      params =
        Common.PaymentParams
          { amount = totalPrice,
            currency = "INR",
            transaction_id = "plink_13234212",
            transaction_status = Common.PAYMENT_LINK_CREATED
          }

  Common.Payment {..}

parkingSupportEmail, parkingSupportPhoneNumber :: Text
parkingSupportEmail = "supportbpp@nomail.com"
parkingSupportPhoneNumber = "+9123456789"

parkingContact :: Contact
parkingContact =
  Contact
    { phone = parkingSupportPhoneNumber,
      email = parkingSupportEmail
    }
