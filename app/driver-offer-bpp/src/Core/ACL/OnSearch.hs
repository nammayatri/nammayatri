module Core.ACL.OnSearch where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import Beckn.Types.Id (ShortId)
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Organization as DOrg

data DOnSearchReq = DOnSearchReq
  { transporterInfo :: TransporterInfo,
    quotes :: [DQuote.DriverQuote]
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DOrg.Organization,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

mkOnSearchMessage ::
  DOnSearchReq ->
  OnSearch.OnSearchMessage
mkOnSearchMessage DOnSearchReq {..} = do
  let provider =
        OnSearch.Provider
          { id = transporterInfo.shortId.getShortId,
            descriptor = undefined,
            locations = undefined,
            categories = undefined,
            --            name = transporterInfo.name,
            --            category_id = "ONE_WAY",
            items = map mkItem quotes,
            offers = undefined,
            add_ons = undefined,
            fulfillments = undefined,
            contacts = transporterInfo.contacts,
            tags = undefined,
            payment = undefined
            --            rides_inprogress = transporterInfo.ridesInProgress,
            --            rides_completed = transporterInfo.ridesCompleted,
            --            rides_confirmed = transporterInfo.ridesConfirmed
          }
  OnSearch.OnSearchMessage $
    OnSearch.Catalog
      { bpp_providers = [provider],
        bpp_descriptor = undefined
      }

mkItem :: DQuote.DriverQuote -> OnSearch.Item
mkItem q =
  OnSearch.Item
    { --    id = q.id.getId,
      --      vehicle_variant = show q.vehicleVariant,
      --      estimated_price = estimated_price_,
      price = price_
      --      discount = Nothing,
      --      discounted_price = estimated_price_,
      --      nearest_driver_distance = Just $ OnSearch.DecimalValue $ toRational q.distanceToPickup.getMeters,
      --      baseDistance = Nothing,
      --      baseDurationHr = Nothing,
      --      descriptions = Nothing
    }
  where
    --    estimated_price_ = OnSearch.Price $ realToFrac $ q.baseFare + fromMaybe 0 q.extraFareSelected
    price_ =
      OnSearch.ItemPrice
        { currency = "INR"
        }
