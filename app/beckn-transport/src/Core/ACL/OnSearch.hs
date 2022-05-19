module Core.ACL.OnSearch (mkOnSearchMessage) where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Types.Quote as DQuote

mkOnSearchMessage ::
  DOnSearch.DOnSearchRes ->
  OnSearch.OnSearchMessage
mkOnSearchMessage DOnSearch.DOnSearchRes {..} = do
  let provider =
        OnSearch.Provider
          { id = transporterInfo.shortId.getShortId,
            name = transporterInfo.name,
            category_id = show fareProductType,
            items = map mkItem quotes,
            contacts = transporterInfo.contacts,
            rides_inprogress = transporterInfo.ridesInProgress,
            rides_completed = transporterInfo.ridesCompleted,
            rides_confirmed = transporterInfo.ridesConfirmed
          }
  OnSearch.OnSearchMessage $ OnSearch.Catalog [provider]

mkItem :: DQuote.Quote -> OnSearch.Item
mkItem DQuote.Quote {..} = do
  let (distanceToNearestDriver, baseDistance, baseDuration, descriptions) =
        case quoteDetails of
          DQuote.OneWayDetails details ->
            (Just details.distanceToNearestDriver, Nothing, Nothing, Nothing)
          DQuote.RentalDetails details ->
            (Nothing, Just details.baseDistance.getKilometers, Just details.baseDuration.getHours, Just details.descriptions)
  OnSearch.Item
    { id = id.getId,
      vehicle_variant = show vehicleVariant,
      estimated_price = OnSearch.Price $ realToFrac estimatedFare,
      discount = OnSearch.Price . realToFrac <$> discount,
      discounted_price = OnSearch.Price $ realToFrac estimatedTotalFare,
      nearest_driver_distance = realToFrac <$> distanceToNearestDriver,
      ..
    }
