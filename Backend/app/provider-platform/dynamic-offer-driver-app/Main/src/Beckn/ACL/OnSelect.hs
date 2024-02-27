{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSelect where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Payment
import BecknV2.Utils
import qualified Data.List as L
import qualified Data.Text as T
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Merchant as DM
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import Kernel.Types.Id (ShortId)
import Kernel.Utils.Common
import SharedLogic.FareCalculator (mkFareParamsBreakups)

data DOnSelectReq = DOnSelectReq
  { transporterInfo :: TransporterInfo,
    searchRequest :: SearchRequest,
    driverQuote :: DQuote.DriverQuote,
    now :: UTCTime
  }

data TransporterInfo = TransporterInfo
  { merchantShortId :: ShortId DM.Merchant,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

mkOnSelectMessageV2 ::
  Bool ->
  DBC.BecknConfig ->
  DM.Merchant ->
  DOnSelectReq ->
  Spec.OnSelectReqMessage
mkOnSelectMessageV2 isValueAddNP bppConfig merchant req@DOnSelectReq {..} = do
  let fulfillments = [mkFulfillmentV2 req driverQuote isValueAddNP]
  let paymentV2 = mkPaymentV2 bppConfig merchant driverQuote
  Spec.OnSelectReqMessage $
    Just
      Spec.Order
        { orderFulfillments = Just fulfillments,
          orderItems = Just $ map (\fulf -> mkItemV2 fulf driverQuote transporterInfo isValueAddNP) fulfillments,
          orderQuote = Just $ mkQuoteV2 driverQuote req.now,
          orderPayments = Just [paymentV2],
          orderProvider = mkProvider bppConfig,
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderId = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Nothing,
          orderUpdatedAt = Nothing
        }

mkFulfillmentV2 :: DOnSelectReq -> DQuote.DriverQuote -> Bool -> Spec.Fulfillment
mkFulfillmentV2 dReq quote isValueAddNP = do
  Spec.Fulfillment
    { fulfillmentId = Just quote.id.getId,
      fulfillmentStops = Utils.mkStops' dReq.searchRequest.fromLocation dReq.searchRequest.toLocation Nothing,
      fulfillmentVehicle = Just $ mkVehicleV2 quote,
      fulfillmentType = Just $ show Enums.DELIVERY,
      fulfillmentAgent = Just $ mkAgentV2 quote isValueAddNP,
      fulfillmentCustomer = Nothing,
      fulfillmentState = Nothing,
      fulfillmentTags = Nothing
    }

mkPaymentV2 :: DBC.BecknConfig -> DM.Merchant -> DQuote.DriverQuote -> Spec.Payment
mkPaymentV2 bppConfig merchant driverQuote = do
  let amount = fromIntegral (driverQuote.estimatedFare.getMoney)
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bppConfig.paymentParamsJson
  mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID (Just amount) Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee

mkVehicleV2 :: DQuote.DriverQuote -> Spec.Vehicle
mkVehicleV2 quote =
  let (category, variant) = Utils.castVariant quote.vehicleVariant
   in Spec.Vehicle
        { vehicleCategory = Just category,
          vehicleVariant = Just variant,
          vehicleColor = Nothing,
          vehicleMake = Nothing,
          vehicleModel = Nothing,
          vehicleRegistration = Nothing
        }

mkAgentV2 :: DQuote.DriverQuote -> Bool -> Spec.Agent
mkAgentV2 quote isValueAddNP =
  Spec.Agent
    { agentContact = Nothing,
      agentPerson = Just $ mkAgentPersonV2 quote isValueAddNP
    }

mkAgentPersonV2 :: DQuote.DriverQuote -> Bool -> Spec.Person
mkAgentPersonV2 quote isValueAddNP =
  Spec.Person
    { personId = Nothing,
      personImage = Nothing,
      personName = Just quote.driverName,
      personTags =
        if isValueAddNP
          then Just . L.singleton $ mkAgentTagsV2 quote
          else Nothing
    }

mkAgentTagsV2 :: DQuote.DriverQuote -> Spec.TagGroup
mkAgentTagsV2 quote =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor = Just $ Spec.Descriptor (Just $ show Tags.AGENT_INFO) (Just "Agent Info") Nothing,
      tagGroupList = Just $ mkAgentTagList quote
    }

mkAgentTagList :: DQuote.DriverQuote -> [Spec.Tag]
mkAgentTagList quote =
  [ Spec.Tag
      { tagDisplay = (\_ -> Just False) =<< quote.driverRating,
        tagDescriptor =
          Just
            Spec.Descriptor
              { descriptorCode = (\_ -> Just $ show Tags.RATING) =<< quote.driverRating,
                descriptorName = (\_ -> Just "Agent Rating") =<< quote.driverRating,
                descriptorShortDesc = Nothing
              },
        tagValue = (\rating -> Just $ show $ rating.getCenti) =<< quote.driverRating
      },
    Spec.Tag
      { tagDisplay = Just False,
        tagDescriptor =
          Just
            Spec.Descriptor
              { descriptorCode = Just $ show Tags.DURATION_TO_PICKUP_IN_S,
                descriptorName = Just "Agent Duration to Pickup in Seconds",
                descriptorShortDesc = Nothing
              },
        tagValue = Just $ show $ quote.durationToPickup.getSeconds
      }
  ]

mkItemV2 :: Spec.Fulfillment -> DQuote.DriverQuote -> TransporterInfo -> Bool -> Spec.Item
mkItemV2 fulfillment quote provider isValueAddNP = do
  let fulfillmentId = fulfillment.fulfillmentId & fromMaybe (error $ "It should never happen as we have created fulfillment:-" <> show fulfillment)
  Spec.Item
    { itemId = Just $ Common.mkItemId provider.merchantShortId.getShortId quote.vehicleVariant,
      itemFulfillmentIds = Just [fulfillmentId],
      itemPrice = Just $ mkPriceV2 quote,
      itemTags = Just . L.singleton =<< mkItemTagsV2 quote isValueAddNP,
      itemDescriptor = mkItemDescriptor quote,
      itemLocationIds = Nothing,
      itemPaymentIds = Nothing
    }

mkItemDescriptor :: DQuote.DriverQuote -> Maybe Spec.Descriptor
mkItemDescriptor res =
  Just
    Spec.Descriptor
      { descriptorCode = Just "RIDE",
        descriptorShortDesc = Just $ show res.vehicleVariant,
        descriptorName = Just $ show res.vehicleVariant
      }

mkPriceV2 :: DQuote.DriverQuote -> Spec.Price
mkPriceV2 quote =
  Spec.Price
    { priceCurrency = Just "INR",
      priceValue = Just $ show $ quote.estimatedFare,
      priceMaximumValue = Nothing,
      priceMinimumValue = Nothing,
      priceOfferedValue = Nothing,
      priceComputedValue = Nothing
    }

mkItemTagsV2 :: DQuote.DriverQuote -> Bool -> Maybe Spec.TagGroup
mkItemTagsV2 quote isValueAddNP
  | not isValueAddNP = Nothing
  | otherwise =
    Just $
      Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor = Just $ Spec.Descriptor (Just $ show Tags.GENERAL_INFO) (Just "General Info") Nothing,
          tagGroupList = Just $ mkItemTagList quote
        }

mkItemTagList :: DQuote.DriverQuote -> [Spec.Tag]
mkItemTagList quote =
  [ Spec.Tag
      { tagDisplay = (\_ -> Just False) =<< quote.specialLocationTag,
        tagDescriptor =
          Just
            Spec.Descriptor
              { descriptorCode = (\_ -> Just $ show Tags.SPECIAL_LOCATION_TAG) =<< quote.specialLocationTag,
                descriptorName = (\_ -> Just "Special Zone Tag") =<< quote.specialLocationTag,
                descriptorShortDesc = Nothing
              },
        tagValue = quote.specialLocationTag
      },
    Spec.Tag
      { tagDisplay = Just False,
        tagDescriptor =
          Just
            Spec.Descriptor
              { descriptorCode = Just $ show Tags.DISTANCE_TO_NEAREST_DRIVER_METER,
                descriptorName = Just "Distance To Nearest Driver In Meters",
                descriptorShortDesc = Nothing
              },
        tagValue = Just $ show $ quote.distanceToPickup.getMeters
      }
  ]

mkQuoteV2 :: DQuote.DriverQuote -> UTCTime -> Spec.Quotation
mkQuoteV2 quote now = do
  let nominalDifferenceTime = diffUTCTime quote.validTill now
  Spec.Quotation
    { quotationBreakup = Just $ mkQuoteBreakupInner quote,
      quotationPrice = mkQuotationPrice quote,
      quotationTtl = Just $ formatTimeDifference nominalDifferenceTime
    }

mkQuoteBreakupInner :: DQuote.DriverQuote -> [Spec.QuotationBreakupInner]
mkQuoteBreakupInner quote = do
  let fareParams = mkFareParamsBreakups mkBreakupPrice mkQuotationBreakupInner quote.fareParams
   in filter filterRequiredBreakups fareParams
  where
    mkBreakupPrice money =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just "INR",
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just $ encodeToText money
          }
    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }
    filterRequiredBreakups breakup =
      breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DISTANCE_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)

mkQuotationPrice :: DQuote.DriverQuote -> Maybe Spec.Price
mkQuotationPrice quote =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText quote.estimatedFare,
        priceValue = Just $ encodeToText quote.estimatedFare
      }

mkProvider :: DBC.BecknConfig -> Maybe Spec.Provider
mkProvider becknConfig = do
  return $
    Spec.Provider
      { providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just $ becknConfig.subscriberId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing
      }
