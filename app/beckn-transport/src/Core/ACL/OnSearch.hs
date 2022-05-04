module Core.ACL.OnSearch (mkOnSearchMessage) where

import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Action.Beckn.Search.OneWay as DOneWaySearch
import qualified Domain.Action.Beckn.Search.Rental as DRentalSearch
import EulerHS.Prelude hiding (state)

mkOnSearchMessage ::
  DSearch.DOnSearchReq ->
  OnSearch.OnSearchMessage
mkOnSearchMessage DSearch.DOnSearchReq {..} = do
  let items = case quotesInfo of
        DSearch.OneWay oneWayQuotesInfo -> map mkOneWayItem oneWayQuotesInfo
        DSearch.Rental rentalQuotesInfo -> map mkRentalItem rentalQuotesInfo
  let provider =
        OnSearch.Provider
          { id = transporterInfo.shortId.getShortId,
            name = transporterInfo.name,
            items = items,
            contacts = transporterInfo.contacts,
            rides_inprogress = transporterInfo.ridesInProgress,
            rides_completed = transporterInfo.ridesCompleted,
            rides_confirmed = transporterInfo.ridesConfirmed
          }
  OnSearch.OnSearchMessage $ OnSearch.Catalog [provider]

mkOneWayItem :: DOneWaySearch.QuoteInfo -> OnSearch.Item
mkOneWayItem DOneWaySearch.QuoteInfo {..} =
  OnSearch.Item
    { id = quoteId.getId,
      category_id = show fareProductType,
      vehicle_variant = show vehicleVariant,
      estimated_price = OnSearch.Price $ realToFrac estimatedFare,
      discount = OnSearch.Price . realToFrac <$> discount,
      discounted_price = OnSearch.Price $ realToFrac estimatedTotalFare,
      nearest_driver_distance = Just $ realToFrac distanceToNearestDriver,
      baseDistance = Nothing,
      baseDurationHr = Nothing,
      descriptions = Nothing
    }

mkRentalItem :: DRentalSearch.QuoteInfo -> OnSearch.Item
mkRentalItem DRentalSearch.QuoteInfo {..} =
  OnSearch.Item
    { id = quoteId.getId,
      category_id = show fareProductType,
      vehicle_variant = show vehicleVariant,
      estimated_price = OnSearch.Price $ realToFrac estimatedFare,
      discount = OnSearch.Price . realToFrac <$> discount,
      discounted_price = OnSearch.Price $ realToFrac estimatedTotalFare,
      nearest_driver_distance = Nothing,
      baseDistance = Just baseDistance,
      baseDurationHr = Just baseDurationHr,
      descriptions = Just descriptions
    }
