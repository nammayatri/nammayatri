{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSelect where

-- import Beckn.ACL.Common
-- import Beckn.Types.Core.Taxi.Common.Gps as Common
-- import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Common

import Beckn.ACL.Common
import qualified Beckn.Types.Core.Taxi.OnSelect as OS
-- import qualified Data.Text as T
-- import Data.Time (diffUTCTime)
-- import Data.Time.Format.ISO8601
-- import Data.Time.LocalTime (calendarTimeTime)
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Merchant as DM
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import Kernel.Types.Id (ShortId)
-- import qualified Domain.Types.FareParameters as DFParams
import SharedLogic.FareCalculator (mkBreakupList)

-- import qualified Kernel.Utils.Common

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

-- driverOfferCategory :: OS.Category
-- driverOfferCategory =
--   OS.Category
--     { id = OS.DRIVER_OFFER,
--       descriptor =
--         OS.Descriptor
--           { name = ""
--           }
--     }

mkOnSelectMessage ::
  DOnSelectReq ->
  OS.OnSelectMessage
mkOnSelectMessage req@DOnSelectReq {..} = do
  -- let quoteEntitiesList :: QuoteEntities
  --     quote = mkQuoteEntities req driverQuote
  let fulfillment = mkFulfillment req driverQuote
      -- fulfillments_ = quote.fulfillment quote
      -- categories_ = [driverOfferCategory]
      -- offers_ = mapMaybe (.offer) quoteEntitiesList
      item = mkItem fulfillment.id driverQuote transporterInfo
      items = [item]
      quote = mkQuote driverQuote req.now
      -- add_ons = []
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
          -- descriptor = OS.Descriptor {name = transporterInfo.name},
          -- locations = [],
          -- categories = categories_
          -- items = items_,
          -- offers = offers_,
          -- add_ons = [],
          -- fulfillment = fulfillment,
          -- contacts = transporterInfo.contacts,
          -- quote = mkQuote driverQuote,
          -- tags =
          --   OS.ProviderTags
          --     { rides_inprogress = transporterInfo.ridesInProgress,
          --       rides_completed = transporterInfo.ridesCompleted,
          --       rides_confirmed = transporterInfo.ridesConfirmed
          --     }
          -- payment =
          --   OS.Payment
          --     { collected_by = "BPP",
          --       _type = OS.ON_FULFILLMENT,
          --       time = OS.TimeDuration "P2A" -- FIXME: what is this?
          --     }
          }
  OS.OnSelectMessage $
    OS.Order {..}

-- data QuoteEntities = QuoteEntities
--   { category :: OS.Category,
--     -- offer :: Maybe OS.Offer,
--     item :: OS.Item
--   }

-- mkQuoteEntities :: DOnSelectReq -> DQuote.DriverQuote -> QuoteEntities
-- mkQuoteEntities dReq quote = do
--   let fulfillment = mkFulfillment dReq quote
--       category = driverOfferCategory
--       -- offer = Nothing
--       item = mkItem fulfillment.id quote
--   QuoteEntities {..}

mkFulfillment :: DOnSelectReq -> DQuote.DriverQuote -> OS.FulfillmentInfo
mkFulfillment dReq quote = do
  let fromLocation = dReq.searchRequest.fromLocation
  let toLocation = dReq.searchRequest.toLocation
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
            -- Just $
            --   OS.AgentTags
            --     { agent_info_rating = (\rating -> Just $ show $ rating.getCenti) =<< quote.driverRating,
            --       agent_info_duration_to_pickup_in_s = Just $ show $ quote.durationToPickup.getSeconds
            --     }
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

-- mkCustomerTipTags =
--   Select.TagGroup
--     { display = False,
--       code = "customer_tip_info",
--       name = "Customer Tip Info",
--       list =
--         [ Select.Tag
--             { display = (\_ -> Just False) =<< res.customerExtraFee,
--               code = (\_ -> Just "customer_tip") =<< res.customerExtraFee,
--               name = (\_ -> Just "Customer Tip") =<< res.customerExtraFee,
--               value = (\charges -> Just $ show charges.getMoney) =<< res.customerExtraFee
--             }
--         ]
--     }

mkItem :: Text -> DQuote.DriverQuote -> TransporterInfo -> OS.Item
mkItem fulfillmentId q provider =
  OS.Item
    { id = mkItemId provider.merchantShortId.getShortId q.vehicleVariant,
      -- category_id = driverOfferCategory.id,
      fulfillment_id = fulfillmentId,
      -- offer_id = Nothing,
      price = mkPrice q,
      -- descriptor =
      --   OS.ItemDescriptor
      --     { name = "",
      --       code =
      --         OS.ItemCode
      --           { fareProductType = OS.DRIVER_OFFER,
      --             vehicleVariant = castVariant q.vehicleVariant,
      --             distance = Nothing,
      --             duration = Nothing
      --           }
      --     },
      -- quote_terms = [],
      tags = Just $ OS.TG [mkItemTags]
      -- Just $
      --   OS.ItemTags
      --     { distance_to_nearest_driver = OS.DecimalValue $ toRational q.distanceToPickup.getMeters,
      --       special_location_tag = q.specialLocationTag
      --     },
      -- base_distance = Nothing,
      -- base_duration = Nothing,
      -- driver_name = Just q.driverName,
      -- duration_to_pickup = Just q.durationToPickup.getSeconds,
      -- valid_till = Just q.validTill
      -- rating = q.driverRating
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

-- where
--   price_ = do
--     let value_ = fromIntegral q.estimatedFare
--     OS.ItemPrice
--       { currency = "INR",
--         value = value_,
--         offered_value = value_
--       }

mkPrice :: DQuote.DriverQuote -> OS.Price
mkPrice quote =
  let value_ = fromIntegral quote.estimatedFare
   in OS.Price
        { currency = "INR",
          value = value_
          -- offered_value = value_
        }

mkQuote :: DQuote.DriverQuote -> UTCTime -> OS.Quote
mkQuote driverQuote _ = do
  let currency = "INR"
      breakup_ =
        mkBreakupList (OS.Price currency . fromIntegral) OS.PriceBreakup driverQuote.fareParams
          & filter filterRequiredBreakups
  -- let nominalDifferenceTime = diffUTCTime now driverQuote.validTill
  -- let diffDuration = calendarTimeTime nominalDifferenceTime
  -- let iso8601Duration = formatShow iso8601Format diffDuration
  OS.Quote
    { price = mkPrice driverQuote,
      ttl = Just $ show driverQuote.validTill, --------- todo
      breakup = breakup_
    }
  where
    filterRequiredBreakups breakup =
      breakup.title == "BASE_FARE"
        || breakup.title == "DEAD_KILOMETER_FARE"
        || breakup.title == "EXTRA_DISTANCE_FARE"
        || breakup.title == "DRIVER_SELECTED_FARE"
        || breakup.title == "CUSTOMER_SELECTED_FARE"
        || breakup.title == "TOTAL_FARE"

-- mkQuoteBreakupList :: DQuote.DriverQuote -> [Maybe OS.PriceBreakup]
-- mkQuoteBreakupList driverQuote =
--   mkBreakupList
--   let parameters = driverQuote.fareParams
--   in
--   [
--     Just $ OS.PriceBreakup {
--         title = "BASE_FARE",
--         price = OS.Price
--           { currency = "INR",
--             value = OS.DecimalValue $ toRational parameters.baseFare.getMoney,
--             offered_value = OS.DecimalValue $ toRational parameters.baseFare.getMoney
--           }
--       },
--     if isJust parameters.customerExtraFee then Just $ OS.PriceBreakup {
--         title = "BASE_FARE",
--         price = OS.Price
--           { currency = "INR",
--             value = OS.DecimalValue $ toRational parameters.baseFare.getMoney,
--             offered_value = OS.DecimalValue $ toRational parameters.baseFare.getMoney
--           }
--       } else Nothing,
--   ]

-- fareParams :: Params.FareParameters,

-- data FareParameters = FareParameters
--   { id :: Id FareParameters,
--     driverSelectedFare :: Maybe Money,
--     customerExtraFee :: Maybe Money,
--     serviceCharge :: Maybe Money,
--     govtCharges :: Maybe Money,
--     baseFare :: Money,
--     waitingCharge :: Maybe Money,
--     nightShiftCharge :: Maybe Money,
--     nightShiftRateIfApplies :: Maybe Double,
--     fareParametersDetails :: FareParametersDetails
--   }

-- data PriceBreakup = PriceBreakup
--   { title :: Text,
--     price :: Price
--   }
--   deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- data Price = Price
--   { currency :: Text,
--     value :: DecimalValue,
--     offered_value :: DecimalValue
--   }
--   deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- -- (fromIntegral $ div (fromEnum . nominalDiffTimeToSeconds $ latency) 1000000000000)
-- -- let Duration =
-- -- calendarTimeTime

--       baseFareCaption = "BASE_FARE"
--       serviceChargeCaption = "SERVICE_CHARGE"
--       mkSelectedFareCaption = "DRIVER_SELECTED_FARE"
--       customerExtraFareCaption = "CUSTOMER_SELECTED_FARE"
--       totalFareCaption = "TOTAL_FARE"
--       nightShiftCaption = "NIGHT_SHIFT_CHARGE"
--       waitingChargesCaption = "WAITING_OR_PICKUP_CHARGES"
--       mbFixedGovtRateCaption = "FIXED_GOVERNMENT_RATE"
