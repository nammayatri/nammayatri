-- | Smart constructors for Beckn Spec types.
-- All fields default to @Nothing@. Use record update syntax to set desired fields:
--
-- @
-- emptyFulfillment { fulfillmentId = Just "123", fulfillmentType = Just "DELIVERY" }
-- @
module BecknV2.OnDemand.Utils.Constructors where

import BecknV2.OnDemand.Types
import Data.Maybe

-- | An empty 'Fulfillment' with all fields set to @Nothing@.
emptyFulfillment :: Fulfillment
emptyFulfillment =
  Fulfillment
    { fulfillmentAgent = Nothing,
      fulfillmentCustomer = Nothing,
      fulfillmentId = Nothing,
      fulfillmentState = Nothing,
      fulfillmentStops = Nothing,
      fulfillmentTags = Nothing,
      fulfillmentType = Nothing,
      fulfillmentVehicle = Nothing
    }

-- | An empty 'Vehicle' with all fields set to @Nothing@.
emptyVehicle :: Vehicle
emptyVehicle =
  Vehicle
    { vehicleCategory = Nothing,
      vehicleColor = Nothing,
      vehicleMake = Nothing,
      vehicleModel = Nothing,
      vehicleRegistration = Nothing,
      vehicleVariant = Nothing,
      vehicleCapacity = Nothing
    }

-- | An empty 'Person' with all fields set to @Nothing@.
emptyPerson :: Person
emptyPerson =
  Person
    { personId = Nothing,
      personImage = Nothing,
      personName = Nothing,
      personTags = Nothing
    }

-- | An empty 'Image' with all fields set to @Nothing@.
emptyImage :: Image
emptyImage =
  Image
    { imageHeight = Nothing,
      imageSizeType = Nothing,
      imageUrl = Nothing,
      imageWidth = Nothing
    }

-- | An empty 'Price' with all fields set to @Nothing@.
emptyPrice :: Price
emptyPrice =
  Price
    { priceComputedValue = Nothing,
      priceCurrency = Nothing,
      priceMaximumValue = Nothing,
      priceMinimumValue = Nothing,
      priceOfferedValue = Nothing,
      priceValue = Nothing
    }

-- | An empty 'Descriptor' with all fields set to @Nothing@.
emptyDescriptor :: Descriptor
emptyDescriptor =
  Descriptor
    { descriptorCode = Nothing,
      descriptorName = Nothing,
      descriptorShortDesc = Nothing
    }

-- | An empty 'Stop' with all fields set to @Nothing@.
emptyStop :: Stop
emptyStop =
  Stop
    { stopAuthorization = Nothing,
      stopLocation = Nothing,
      stopId = Nothing,
      stopParentStopId = Nothing,
      stopTime = Nothing,
      stopType = Nothing
    }

-- | An empty 'Order' with all fields set to @Nothing@.
emptyOrder :: Order
emptyOrder =
  Order
    { orderBilling = Nothing,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = Nothing,
      orderPayments = Nothing,
      orderProvider = Nothing,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderUpdatedAt = Nothing
    }

-- | An empty 'Payment' with all fields set to @Nothing@.
emptyPayment :: Payment
emptyPayment =
  Payment
    { paymentCollectedBy = Nothing,
      paymentId = Nothing,
      paymentParams = Nothing,
      paymentStatus = Nothing,
      paymentTags = Nothing,
      paymentTlMethod = Nothing,
      paymentType = Nothing
    }

-- | An empty 'Customer' with all fields set to @Nothing@.
emptyCustomer :: Customer
emptyCustomer =
  Customer
    { customerContact = Nothing,
      customerPerson = Nothing
    }

-- | An empty 'FulfillmentState' with all fields set to @Nothing@.
emptyFulfillmentState :: FulfillmentState
emptyFulfillmentState =
  FulfillmentState
    { fulfillmentStateDescriptor = Nothing
    }

-- | An empty 'Quotation' with all fields set to @Nothing@.
emptyQuotation :: Quotation
emptyQuotation =
  Quotation
    { quotationBreakup = Nothing,
      quotationPrice = Nothing,
      quotationTtl = Nothing
    }

-- | An empty 'Location' with all fields set to @Nothing@.
emptyLocation :: Location
emptyLocation =
  Location
    { locationAddress = Nothing,
      locationAreaCode = Nothing,
      locationCity = Nothing,
      locationCountry = Nothing,
      locationGps = Nothing,
      locationId = Nothing,
      locationState = Nothing,
      locationUpdatedAt = Nothing
    }

-- | An empty 'Agent' with all fields set to @Nothing@.
emptyAgent :: Agent
emptyAgent =
  Agent
    { agentContact = Nothing,
      agentPerson = Nothing
    }

-- | An empty 'Item' with all fields set to @Nothing@.
emptyItem :: Item
emptyItem =
  Item
    { itemCategoryIds = Nothing,
      itemDescriptor = Nothing,
      itemFulfillmentIds = Nothing,
      itemId = Nothing,
      itemLocationIds = Nothing,
      itemPaymentIds = Nothing,
      itemPrice = Nothing,
      itemTags = Nothing
    }

-- | An empty 'Provider' with all fields set to @Nothing@.
emptyProvider :: Provider
emptyProvider =
  Provider
    { providerCategories = Nothing,
      providerDescriptor = Nothing,
      providerFulfillments = Nothing,
      providerId = Nothing,
      providerItems = Nothing,
      providerLocations = Nothing,
      providerPayments = Nothing
    }
