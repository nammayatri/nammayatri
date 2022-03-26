module API.Order where

import API.Common
import Beckn.Prelude
import qualified Domain.Address as DAddress
import qualified Domain.Delivery as DDelivery
import qualified Domain.Person as DPerson
import qualified Types.Beckn.Address as Address
import qualified Types.Beckn.Billing as Billing
import qualified Types.Beckn.Contact as Contact
import qualified Types.Beckn.DecimalValue as DecimalValue
import qualified Types.Beckn.Fulfillment as Fulfillment
import qualified Types.Beckn.FulfillmentDetails as FDetails
import qualified Types.Beckn.Gps as Gps
import qualified Types.Beckn.Location as Location
import qualified Types.Beckn.Order as Order
import qualified Types.Beckn.Payment as Payment
import qualified Types.Beckn.Person as Person

mkOrder ::
  DDelivery.Delivery ->
  Order.Order
mkOrder DDelivery.Delivery {..} = do
  Order.Order
    { state = mapTaskStateToOrderState status,
      items = [Order.OrderItem {id = show categoryId}],
      billing =
        Billing.Billing --why do we need this billing?
          { name = sender.name,
            phone = sender.phone
          },
      fulfillment =
        Fulfillment.Fulfillment
          { id = deliveryServiceOrderId,
            tracking = False, --do we use it anywhere?
            start = mkFulfillmentDetails sender pickupAddress,
            end = mkFulfillmentDetails receiver dropAddress
          },
      payment = mkPayment deliveryPrice,
      updated_at = updatedAt
    }

mkFulfillmentDetails ::
  DPerson.Person ->
  DAddress.Address ->
  FDetails.FulfillmentDetails
mkFulfillmentDetails DPerson.Person {..} address = do
  FDetails.FulfillmentDetails
    { location = mkLocation address,
      instructions = FDetails.DescriptorInfo <$> address.instructions,
      contact = Contact.Contact {..},
      person = Person.Person {..}
    }

mkLocation :: DAddress.Address -> Location.Location
mkLocation DAddress.Address {..} =
  Location.Location
    { gps = Gps.Gps {..},
      address =
        Address.Address
          { area_code = pincode,
            ..
          }
    }

mkPayment :: Maybe Float -> Payment.Payment
mkPayment estimated_price =
  Payment.Payment
    { params =
        Payment.Params
          { amount = DecimalValue.DecimalValue . show <$> estimated_price,
            currency = "INR"
          },
      _type = Payment.POST_FULFILLMENT
    }
