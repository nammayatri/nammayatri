module Core.ACL.OnSearch (mkOnSearchMessage) where

import Beckn.External.Maps.Types (LatLong (..))
import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.OnSearch.OneWay as DOneWaySearch
import qualified Domain.Action.Beckn.OnSearch.Rental as DRentalSearch
import qualified Domain.Types.FarePolicy.FareProduct as DFP
import qualified Domain.Types.Vehicle as Veh

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
          { id = transporterInfo.shortId.getShortId,
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
  OnSearch.OnSearchMessage $ OnSearch.Catalog catalogDescriptor [provider]

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
            offered_value = fromIntegral estimatedTotalFare
          }
      tags =
        OnSearch.ItemTags
          { distance_to_nearest_driver = realToFrac distanceToNearestDriver
          }
  OnSearch.Item
    { category_id = OnSearch.ONE_WAY_TRIP,
      base_distance = Nothing,
      base_duration = Nothing,
      quote_terms = [],
      tags = Just tags,
      ..
    }

mkOneWayFulfillment :: DOneWaySearch.QuoteInfo -> Int -> OnSearch.FulfillmentInfo
mkOneWayFulfillment DOneWaySearch.QuoteInfo {..} fulfillmentId = do
  OnSearch.FulfillmentInfo
    { id = show fulfillmentId,
      vehicle = OnSearch.FulfillmentVehicle $ castVehicleVariant vehicleVariant,
      start =
        OnSearch.StartInfo
          { location = OnSearch.Location $ mkGps fromLocation,
            time = OnSearch.TimeTimestamp startTime
          },
      end = Just . OnSearch.StopInfo . OnSearch.Location $ mkGps toLocation
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
          { location = OnSearch.Location $ mkGps fromLocation,
            time = OnSearch.TimeTimestamp startTime
          },
      end = Nothing
    }

mkRentalItem :: DRentalSearch.QuoteInfo -> Maybe Text -> Text -> OnSearch.Item
mkRentalItem DRentalSearch.QuoteInfo {..} offer_id fulfillment_id = do
  let vehVar = castVehicleVariant vehicleVariant
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
            offered_value = fromIntegral estimatedTotalFare
          }
  OnSearch.Item
    { category_id = OnSearch.RENTAL_TRIP,
      base_distance = Just baseDistance,
      base_duration = Just baseDuration,
      quote_terms = descriptions,
      tags = Nothing,
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
