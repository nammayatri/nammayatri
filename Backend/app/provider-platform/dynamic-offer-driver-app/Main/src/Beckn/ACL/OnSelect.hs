{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSelect where

import Beckn.ACL.Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.OnSelect as OS
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (diffUTCTime, nominalDiffTimeToSeconds)
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Location as Location
import qualified Domain.Types.Merchant as DM
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Id (ShortId)
import Kernel.Utils.Common (encodeToText)
import SharedLogic.FareCalculator (mkBreakupList)

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

mkOnSelectMessage ::
  DOnSelectReq ->
  OS.OnSelectMessage
mkOnSelectMessage req@DOnSelectReq {..} = do
  let fulfillment = mkFulfillment req driverQuote
      item = mkItem fulfillment.id driverQuote transporterInfo
      items = [item]
      quote = mkQuote driverQuote req.now
      payment =
        OS.Payment
          { params =
              OS.PaymentParams
                { collected_by = OS.BPP,
                  instrument = Nothing,
                  currency = "INR",
                  amount = Nothing
                },
            _type = OS.ON_FULFILLMENT,
            uri = Nothing
          }
  let provider =
        OS.Provider
          { id = driverQuote.driverId.getId
          }
  OS.OnSelectMessage $
    OS.Order {..}

mkOnSelectMessageV2 ::
  DOnSelectReq ->
  Spec.OnSelectReqMessage
mkOnSelectMessageV2 req@DOnSelectReq {..} = do
  let fulfillments = [mkFulfillmentV2 req driverQuote]
  Spec.OnSelectReqMessage $
    Just $
      Spec.Order
        { orderFulfillments = Just fulfillments,
          orderItems = Just $ map (\fulf -> mkItemV2 fulf driverQuote transporterInfo) fulfillments,
          orderQuote = Just $ mkQuoteV2 driverQuote req.now,
          orderPayments = Just [mkPaymentV2],
          orderProvider = Just $ mkProviderV2 driverQuote,
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderId = Nothing,
          orderStatus = Nothing
        }

mkFulfillment :: DOnSelectReq -> DQuote.DriverQuote -> OS.FulfillmentInfo
mkFulfillment dReq quote = do
  let fromLocation = dReq.searchRequest.fromLocation
  let toLocation = dReq.searchRequest.toLocation -- have to take last or all ?
  OS.FulfillmentInfo
    { id = quote.estimateId.getId,
      start =
        OS.StartInfo
          { location = makeLocation fromLocation
          },
      end =
        OS.StopInfo
          { location = makeLocation toLocation
          },
      vehicle =
        OS.Vehicle
          { category = castVariant quote.vehicleVariant
          },
      _type = OS.RIDE,
      agent =
        OS.Agent
          { name = Just quote.driverName,
            rateable = Just True,
            tags = OS.TG [mkAgentTags]
          }
    }
  where
    mkAgentTags =
      OS.TagGroup
        { display = False,
          code = "agent_info",
          name = "Agent Info",
          list =
            [ OS.Tag
                { display = (\_ -> Just False) =<< quote.driverRating,
                  code = (\_ -> Just "rating") =<< quote.driverRating,
                  name = (\_ -> Just "Agent Rating") =<< quote.driverRating,
                  value = (\rating -> Just $ show $ rating.getCenti) =<< quote.driverRating
                },
              OS.Tag
                { display = Just False,
                  code = Just "duration_to_pickup_in_s",
                  name = Just "Agent Duration to Pickup in Seconds",
                  value = Just $ show $ quote.durationToPickup.getSeconds
                }
            ]
        }

mkFulfillmentV2 :: DOnSelectReq -> DQuote.DriverQuote -> Spec.Fulfillment
mkFulfillmentV2 dReq quote = do
  Spec.Fulfillment
    { fulfillmentId = Just quote.estimateId.getId,
      fulfillmentStops = mkStops dReq.searchRequest.fromLocation dReq.searchRequest.toLocation,
      fulfillmentVehicle = Just $ mkVehicleV2 quote,
      fulfillmentType = Just "RIDE",
      fulfillmentAgent = Just $ mkAgentV2 quote,
      fulfillmentCustomer = Nothing,
      fulfillmentState = Nothing,
      fulfillmentTags = Nothing
    }

mkPaymentV2 :: Spec.Payment
mkPaymentV2 =
  Spec.Payment
    { paymentParams = Just $ mkPaymentParamsV2,
      paymentType = Just "ON_FULFILLMENT",
      paymentCollectedBy = Just "BPP",
      paymentId = Nothing,
      paymentStatus = Nothing,
      paymentTags = Nothing
    }

mkPaymentParamsV2 :: Spec.PaymentParams
mkPaymentParamsV2 =
  Spec.PaymentParams
    { paymentParamsCurrency = Just "INR",
      paymentParamsAmount = Nothing,
      paymentParamsBankAccountNumber = Nothing,
      paymentParamsBankCode = Nothing,
      paymentParamsVirtualPaymentAddress = Nothing
    }

mkProviderV2 :: DQuote.DriverQuote -> Spec.Provider
mkProviderV2 quote =
  Spec.Provider
    { providerId = Just quote.driverId.getId,
      providerDescriptor = Nothing,
      providerFulfillments = Nothing,
      providerItems = Nothing,
      providerLocations = Nothing,
      providerPayments = Nothing
    }

mkVehicleV2 :: DQuote.DriverQuote -> Spec.Vehicle
mkVehicleV2 quote =
  Spec.Vehicle
    { vehicleCategory = Just $ Utils.castVariant quote.vehicleVariant,
      vehicleColor = Nothing,
      vehicleMake = Nothing,
      vehicleModel = Nothing,
      vehicleRegistration = Nothing,
      vehicleVariant = Nothing
    }

mkAgentV2 :: DQuote.DriverQuote -> Spec.Agent
mkAgentV2 quote =
  Spec.Agent
    { agentContact = Nothing,
      agentPerson = Just $ mkAgentPersonV2 quote
    }

mkAgentPersonV2 :: DQuote.DriverQuote -> Spec.Person
mkAgentPersonV2 quote =
  Spec.Person
    { personId = Nothing,
      personImage = Nothing,
      personName = Just quote.driverName,
      personTags = Just [mkAgentTagsV2 quote]
    }

mkAgentTagsV2 :: DQuote.DriverQuote -> Spec.TagGroup
mkAgentTagsV2 quote =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor = Just $ Spec.Descriptor (Just "agent_info") (Just "Agent Info") Nothing,
      tagGroupList = Just $ mkAgentTagList quote
    }

mkStops :: Location.Location -> Location.Location -> Maybe [Spec.Stop]
mkStops origin destination =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps d = Gps.Gps {lat = d.lat, lon = d.lon}
   in Just
        [ Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = origin.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = origin.address.areaCode,
                      locationCity = Just $ Spec.City Nothing origin.address.city,
                      locationCountry = Just $ Spec.Country Nothing origin.address.country,
                      locationGps = A.decode $ A.encode originGps,
                      locationState = Just $ Spec.State origin.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "START",
              stopAuthorization = Nothing
            },
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = destination.address.building, -- JAYPAL, Confirm if it is correct to put it here
                      locationAreaCode = destination.address.areaCode,
                      locationCity = Just $ Spec.City Nothing destination.address.city,
                      locationCountry = Just $ Spec.Country Nothing destination.address.country,
                      locationGps = A.decode $ A.encode (destinationGps destination),
                      locationState = Just $ Spec.State destination.address.state,
                      locationId = Nothing -- JAYPAL, Not sure what to keep here
                    },
              stopType = Just "END",
              stopAuthorization = Nothing
            }
        ]

mkAgentTagList :: DQuote.DriverQuote -> [Spec.Tag]
mkAgentTagList quote =
  [ Spec.Tag
      { tagDisplay = (\_ -> Just False) =<< quote.driverRating,
        tagDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = (\_ -> Just "rating") =<< quote.driverRating,
                descriptorName = (\_ -> Just "Agent Rating") =<< quote.driverRating,
                descriptorShortDesc = Nothing
              },
        tagValue = (\rating -> Just $ show $ rating.getCenti) =<< quote.driverRating
      },
    Spec.Tag
      { tagDisplay = Just False,
        tagDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just "duration_to_pickup_in_s",
                descriptorName = Just "Agent Duration to Pickup in Seconds",
                descriptorShortDesc = Nothing
              },
        tagValue = Just $ show $ quote.durationToPickup.getSeconds
      }
  ]

mkItem :: Text -> DQuote.DriverQuote -> TransporterInfo -> OS.Item
mkItem fulfillmentId q provider =
  OS.Item
    { id = mkItemId provider.merchantShortId.getShortId q.vehicleVariant,
      fulfillment_id = fulfillmentId,
      price = mkPrice q,
      tags = Just $ OS.TG [mkItemTags]
    }
  where
    mkItemTags =
      OS.TagGroup
        { display = False,
          code = "general_info",
          name = "General Info",
          list =
            [ OS.Tag
                { display = (\_ -> Just False) =<< q.specialLocationTag,
                  code = (\_ -> Just "special_location_tag") =<< q.specialLocationTag,
                  name = (\_ -> Just "Special Zone Tag") =<< q.specialLocationTag,
                  value = q.specialLocationTag
                },
              OS.Tag
                { display = Just False,
                  code = Just "distance_to_nearest_driver_in_m",
                  name = Just "Distance To Nearest Driver In Meters",
                  value = Just $ show $ q.distanceToPickup.getMeters
                },
              OS.Tag
                { display = Just False,
                  code = Just "bpp_quote_id",
                  name = Just "BPP Quote Id",
                  value = Just q.id.getId
                }
            ]
        }

mkItemV2 :: Spec.Fulfillment -> DQuote.DriverQuote -> TransporterInfo -> Spec.Item
mkItemV2 fulfillment quote provider = do
  let fulfillmentId = fromMaybe "" fulfillment.fulfillmentId
  Spec.Item
    { itemId = Just $ provider.merchantShortId.getShortId <> show quote.vehicleVariant,
      itemFulfillmentIds = Just [fulfillmentId],
      itemPrice = Just $ mkPriceV2 quote,
      itemTags = Just [mkItemTagsV2 quote],
      itemDescriptor = Nothing,
      itemLocationIds = Nothing,
      itemPaymentIds = Nothing
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

mkItemTagsV2 :: DQuote.DriverQuote -> Spec.TagGroup
mkItemTagsV2 quote =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor = Just $ Spec.Descriptor (Just "general_info") (Just "General Info") Nothing,
      tagGroupList = Just $ mkItemTagList quote
    }

mkItemTagList :: DQuote.DriverQuote -> [Spec.Tag]
mkItemTagList quote =
  [ Spec.Tag
      { tagDisplay = (\_ -> Just False) =<< quote.specialLocationTag,
        tagDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = (\_ -> Just "special_location_tag") =<< quote.specialLocationTag,
                descriptorName = (\_ -> Just "Special Zone Tag") =<< quote.specialLocationTag,
                descriptorShortDesc = Nothing
              },
        tagValue = quote.specialLocationTag
      },
    Spec.Tag
      { tagDisplay = Just False,
        tagDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just "distance_to_nearest_driver_in_m",
                descriptorName = Just "Distance To Nearest Driver In Meters",
                descriptorShortDesc = Nothing
              },
        tagValue = Just $ show $ quote.distanceToPickup.getMeters
      },
    Spec.Tag
      { tagDisplay = Just False,
        tagDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just "bpp_quote_id",
                descriptorName = Just "BPP Quote Id",
                descriptorShortDesc = Nothing
              },
        tagValue = Just $ quote.id.getId
      }
  ]

mkPrice :: DQuote.DriverQuote -> OS.Price
mkPrice quote =
  let value_ = fromIntegral quote.estimatedFare
   in OS.Price
        { currency = "INR",
          value = value_
        }

mkQuote :: DQuote.DriverQuote -> UTCTime -> OS.Quote
mkQuote driverQuote now = do
  let currency = "INR"
      breakup_ =
        mkBreakupList (OS.Price currency . fromIntegral) OS.PriceBreakup driverQuote.fareParams
          & filter filterRequiredBreakups
  let nominalDifferenceTime = diffUTCTime driverQuote.validTill now
  OS.Quote
    { price = mkPrice driverQuote,
      ttl = Just $ T.pack $ formatTimeDifference nominalDifferenceTime, --------- todo
      breakup = breakup_
    }
  where
    filterRequiredBreakups breakup =
      breakup.title == "BASE_FARE"
        || breakup.title == "SERVICE_CHARGE"
        || breakup.title == "DEAD_KILOMETER_FARE"
        || breakup.title == "EXTRA_DISTANCE_FARE"
        || breakup.title == "DRIVER_SELECTED_FARE"
        || breakup.title == "CUSTOMER_SELECTED_FARE"
        || breakup.title == "TOTAL_FARE"
        || breakup.title == "WAITING_OR_PICKUP_CHARGES"
        || breakup.title == "EXTRA_TIME_FARE"
    formatTimeDifference duration =
      let secondsDiff = div (fromEnum . nominalDiffTimeToSeconds $ duration) 1000000000000
          (hours, remainingSeconds) = divMod secondsDiff (3600 :: Int)
          (minutes, seconds) = divMod remainingSeconds 60
       in "PT" <> show hours <> "H" <> show minutes <> "M" <> show seconds <> "S"

mkQuoteV2 :: DQuote.DriverQuote -> UTCTime -> Spec.Quotation
mkQuoteV2 quote now = do
  let nominalDifferenceTime = diffUTCTime quote.validTill now
  Spec.Quotation
    { quotationBreakup = Just $ mkQuoteBreakupInner quote,
      quotationPrice = mkQuotationPrice quote,
      quotationTtl = Just $ T.pack $ formatTimeDifference nominalDifferenceTime --------- todo
    }
  where
    formatTimeDifference duration =
      let secondsDiff = div (fromEnum . nominalDiffTimeToSeconds $ duration) 1000000000000
          (hours, remainingSeconds) = divMod secondsDiff (3600 :: Int)
          (minutes, seconds) = divMod remainingSeconds 60
       in "PT" <> show hours <> "H" <> show minutes <> "M" <> show seconds <> "S"

mkQuoteBreakupInner :: DQuote.DriverQuote -> [Spec.QuotationBreakupInner]
mkQuoteBreakupInner quote = do
  let fareParams = mkBreakupList mkBreakupPrice mkQuotationBreakupInner quote.fareParams
   in filter filterRequiredBreakups fareParams
  where
    mkBreakupPrice money =
      Just $
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
      breakup.quotationBreakupInnerTitle == Just "BASE_FARE"
        || breakup.quotationBreakupInnerTitle == Just "SERVICE_CHARGE"
        || breakup.quotationBreakupInnerTitle == Just "DEAD_KILOMETER_FARE"
        || breakup.quotationBreakupInnerTitle == Just "EXTRA_DISTANCE_FARE"
        || breakup.quotationBreakupInnerTitle == Just "DRIVER_SELECTED_FARE"
        || breakup.quotationBreakupInnerTitle == Just "CUSTOMER_SELECTED_FARE"
        || breakup.quotationBreakupInnerTitle == Just "TOTAL_FARE"
        || breakup.quotationBreakupInnerTitle == Just "WAITING_OR_PICKUP_CHARGES"
        || breakup.quotationBreakupInnerTitle == Just "EXTRA_TIME_FARE"

mkQuotationPrice :: DQuote.DriverQuote -> Maybe Spec.Price
mkQuotationPrice quote =
  Just $
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Nothing,
        priceValue = Just $ encodeToText quote.estimatedFare
      }
