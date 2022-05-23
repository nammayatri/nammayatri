module Core.ACL.OnSearch (mkOnSearchMessage) where

import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Quote as DQuote
import EulerHS.Prelude hiding (id, state)

mkOnSearchMessage ::
  DSearch.DOnSearchReq ->
  OnSearch.OnSearchMessage
mkOnSearchMessage DSearch.DOnSearchReq {..} = do
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
  let (distanceToNearestDriver, baseDistance, baseDurationHr, descriptions) =
        case quoteDetails of
          DQuote.OneWayDetails details ->
            (Just details.distanceToNearestDriver, Nothing, Nothing, Nothing)
          DQuote.RentalDetails details ->
            (Nothing, Just details.baseDistance, Just details.baseDurationHr, Just details.descriptions)
  OnSearch.Item
    { id = id.getId,
      vehicle_variant = show vehicleVariant,
      estimated_price = OnSearch.Price $ realToFrac estimatedFare,
      discount = OnSearch.Price . realToFrac <$> discount,
      discounted_price = OnSearch.Price $ realToFrac estimatedTotalFare,
      nearest_driver_distance = realToFrac <$> distanceToNearestDriver,
      ..
    }
