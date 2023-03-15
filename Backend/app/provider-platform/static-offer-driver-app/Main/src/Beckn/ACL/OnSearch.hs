{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch (mkOnSearchMessage) where

import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.OnSearch.OneWay as DOneWaySearch
import qualified Domain.Action.Beckn.OnSearch.Rental as DRentalSearch
import qualified Domain.Types.FarePolicy.FareProduct as DFP
import qualified Domain.Types.Vehicle as Veh
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude

mkOnSearchMessage ::
  DOnSearch.DOnSearchRes ->
  OnSearch.OnSearchMessage
mkOnSearchMessage DOnSearch.DOnSearchRes {..} = do
  let (items, fulfillments, offers) = unzip3 $ case quoteInfos of
        DOnSearch.OneWayQuoteInfo oneWayQuotesInfo -> zipWith (curry mkMainOneWayEntities) oneWayQuotesInfo [1, 2 ..]
        DOnSearch.RentalQuoteInfo rentalQuotesInfo -> zipWith (curry mkMainRentalEntities) rentalQuotesInfo [1, 2 ..]
  let catalogDescriptor = OnSearch.Descriptor "Yatri partner"
      provider =
        OnSearch.Provider
          { id = transporterInfo.subscriberId.getShortId,
            descriptor = OnSearch.Descriptor transporterInfo.name,
            locations = [],
            categories = [mkCategory fareProductType],
            contacts = transporterInfo.contacts,
            tags = mkTags transporterInfo,
            add_ons = [],
            payment = mkPayment,
            offers = catMaybes offers,
            ..
          }
  OnSearch.OnSearchMessage $ OnSearch.Catalog catalogDescriptor $ pure provider

--------------------------------------------------------------------------------------------------------
----------------------------------------------------ONE-WAY---------------------------------------------
--------------------------------------------------------------------------------------------------------

mkMainOneWayEntities :: (DOneWaySearch.QuoteInfo, Int) -> (OnSearch.Item, OnSearch.FulfillmentInfo, Maybe OnSearch.Offer)
mkMainOneWayEntities (quoteInfo, quoteNum) = do
  let fulfillment = mkOneWayFulfillment quoteInfo quoteNum
      offer = quoteInfo.discount $> mkOffer quoteNum
      item = mkOneWayItem quoteInfo (offer <&> (.id)) fulfillment.id
  (item, fulfillment, offer)

mkOneWayItem :: DOneWaySearch.QuoteInfo -> Maybe Text -> Text -> OnSearch.Item
mkOneWayItem DOneWaySearch.QuoteInfo {..} offer_id fulfillment_id = do
  let vehVar = castVehicleVariant vehicleVariant
      offered_value = fromIntegral estimatedTotalFare
      descriptor =
        OnSearch.ItemDescriptor
          { name = mkVehicleRideDescription vehVar,
            code =
              OnSearch.ItemCode
                { fareProductType = OnSearch.ONE_WAY_TRIP,
                  vehicleVariant = vehVar,
                  duration = Nothing,
                  distance = Nothing
                }
          }
      price =
        OnSearch.ItemPrice
          { currency = "INR",
            value = fromIntegral estimatedFare,
            offered_value,
            minimum_value = offered_value,
            maximum_value = offered_value,
            value_breakup = []
          }
      tags =
        OnSearch.ItemTags
          { distance_to_nearest_driver = Just $ realToFrac distanceToNearestDriver,
            night_shift_multiplier = Nothing,
            night_shift_start = Nothing,
            night_shift_end = Nothing,
            waiting_charge_per_min = Nothing, -- figure out how to send
            waiting_time_estimated_threshold = Nothing,
            drivers_location = []
          }
  OnSearch.Item
    { category_id = OnSearch.ONE_WAY_TRIP,
      base_distance = Nothing,
      base_duration = Nothing,
      quote_terms = [],
      tags = Just tags,
      id = show OnSearch.ONE_WAY_TRIP,
      ..
    }

mkOneWayFulfillment :: DOneWaySearch.QuoteInfo -> Int -> OnSearch.FulfillmentInfo
mkOneWayFulfillment DOneWaySearch.QuoteInfo {..} fulfillmentId = do
  OnSearch.FulfillmentInfo
    { id = show fulfillmentId,
      vehicle = OnSearch.FulfillmentVehicle $ castVehicleVariant vehicleVariant,
      start =
        OnSearch.StartInfo
          { location =
              OnSearch.Location
                { gps = mkGps fromLocation,
                  address = Nothing
                },
            time = OnSearch.TimeTimestamp startTime
          },
      end =
        Just $
          OnSearch.StopInfo
            { location =
                OnSearch.Location
                  { gps = mkGps toLocation,
                    address = Nothing
                  }
            }
    }

--------------------------------------------------------------------------------------------------------
----------------------------------------------------RENTAL----------------------------------------------
--------------------------------------------------------------------------------------------------------

mkMainRentalEntities :: (DRentalSearch.QuoteInfo, Int) -> (OnSearch.Item, OnSearch.FulfillmentInfo, Maybe OnSearch.Offer)
mkMainRentalEntities (quoteInfo, quoteNum) = do
  let fulfillment = mkRentalFulfillment quoteInfo quoteNum
      offer = quoteInfo.discount $> mkOffer quoteNum
      item = mkRentalItem quoteInfo (offer <&> (.id)) fulfillment.id
  (item, fulfillment, offer)

mkRentalFulfillment :: DRentalSearch.QuoteInfo -> Int -> OnSearch.FulfillmentInfo
mkRentalFulfillment DRentalSearch.QuoteInfo {..} fulfillmentId = do
  OnSearch.FulfillmentInfo
    { id = show fulfillmentId,
      vehicle = OnSearch.FulfillmentVehicle $ castVehicleVariant vehicleVariant,
      start =
        OnSearch.StartInfo
          { location =
              OnSearch.Location
                { gps = mkGps fromLocation,
                  address = Nothing
                },
            time = OnSearch.TimeTimestamp startTime
          },
      end = Nothing
    }

mkRentalItem :: DRentalSearch.QuoteInfo -> Maybe Text -> Text -> OnSearch.Item
mkRentalItem DRentalSearch.QuoteInfo {..} offer_id fulfillment_id = do
  let vehVar = castVehicleVariant vehicleVariant
      offered_value = fromIntegral estimatedTotalFare
      descriptor =
        OnSearch.ItemDescriptor
          { name = mkVehicleRideDescription vehVar,
            code =
              OnSearch.ItemCode
                { fareProductType = OnSearch.RENTAL_TRIP,
                  vehicleVariant = vehVar,
                  duration = Just baseDuration,
                  distance = Just baseDistance
                }
          }
      price =
        OnSearch.ItemPrice
          { currency = "INR",
            value = fromIntegral estimatedFare,
            offered_value,
            minimum_value = offered_value,
            maximum_value = offered_value,
            value_breakup = []
          }
  OnSearch.Item
    { category_id = OnSearch.RENTAL_TRIP,
      base_distance = Just baseDistance,
      base_duration = Just baseDuration,
      quote_terms = descriptions,
      tags = Nothing,
      id = show OnSearch.ONE_WAY_TRIP,
      ..
    }

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

mkVehicleRideDescription :: OnSearch.VehicleVariant -> Text
mkVehicleRideDescription = \case
  OnSearch.SUV -> "SUV ride."
  OnSearch.HATCHBACK -> "Hatchback ride."
  OnSearch.SEDAN -> "Sedan ride."
  OnSearch.AUTO_RICKSHAW -> "Auto-rickshaw ride."

mkCategory :: DFP.FareProductType -> OnSearch.Category
mkCategory DFP.ONE_WAY = do
  OnSearch.Category
    { id = OnSearch.ONE_WAY_TRIP,
      descriptor = OnSearch.Descriptor "One way trip."
    }
mkCategory DFP.RENTAL = do
  OnSearch.Category
    { id = OnSearch.RENTAL_TRIP,
      descriptor = OnSearch.Descriptor "Rental trip."
    }

mkTags :: DOnSearch.TransporterInfo -> OnSearch.ProviderTags
mkTags transporterInfo =
  OnSearch.ProviderTags
    { rides_inprogress = transporterInfo.ridesInProgress,
      rides_completed = transporterInfo.ridesCompleted,
      rides_confirmed = transporterInfo.ridesConfirmed
    }

castVehicleVariant :: Veh.Variant -> OnSearch.VehicleVariant
castVehicleVariant = \case
  Veh.SUV -> OnSearch.SUV
  Veh.HATCHBACK -> OnSearch.HATCHBACK
  Veh.SEDAN -> OnSearch.SEDAN

mkPayment :: OnSearch.Payment
mkPayment =
  OnSearch.Payment
    { collected_by = "BAP",
      time = OnSearch.TimeDuration "P2D",
      _type = OnSearch.ON_FULFILLMENT
    }

mkOffer :: Int -> OnSearch.Offer
mkOffer offerId =
  OnSearch.Offer
    { id = show offerId,
      descriptor =
        OnSearch.Descriptor
          { name = "Discount"
          }
    }

mkGps :: LatLong -> OnSearch.Gps
mkGps LatLong {..} = OnSearch.Gps {..}
