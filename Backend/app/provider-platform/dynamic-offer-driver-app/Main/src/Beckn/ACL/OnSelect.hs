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
import qualified Beckn.Types.Core.Taxi.OnSelect as OS
import qualified Data.Text as T
import Data.Time (diffUTCTime, nominalDiffTimeToSeconds)
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Merchant as DM
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Types.Id (ShortId)
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

mkFulfillment :: DOnSelectReq -> DQuote.DriverQuote -> OS.FulfillmentInfo
mkFulfillment dReq quote = do
  let searchDetails = dReq.searchRequest.searchRequestDetails
  case dReq.searchRequest.tag of
    DSR.ON_DEMAND -> do
      let fromLocation = searchDetails.fromLocation
      let toLocation = searchDetails.toLocation -- have to take last or all ?
      OS.FulfillmentInfo
        { id = quote.estimateId.getId,
          start =
            OS.StartInfo
              { location = makeLocation fromLocation,
                time = OS.TimeTimestamp dReq.now
              },
          end =
            Just $ OS.StopInfo
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
    DSR.RENTAL -> do
      let fromLocation = searchDetails.rentalFromLocation
      OS.FulfillmentInfo
        { id = quote.estimateId.getId,
          start =
            OS.StartInfo
              { location = makeLocation fromLocation,
                time = OS.TimeTimestamp dReq.now
              },
          end = Nothing,
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
    formatTimeDifference duration =
      let secondsDiff = div (fromEnum . nominalDiffTimeToSeconds $ duration) 1000000000000
          (hours, remainingSeconds) = divMod secondsDiff (3600 :: Int)
          (minutes, seconds) = divMod remainingSeconds 60
       in "PT" <> show hours <> "H" <> show minutes <> "M" <> show seconds <> "S"
