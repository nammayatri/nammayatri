module SharedLogic.Search where

import qualified BecknV2.OnDemand.Tags as Beckn
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Storage.Beam.SystemConfigs ()
import Domain.Types.Location as Location
import qualified Tools.Maps as Maps

data SearchRes = SearchRes
  { origin :: SearchReqLocation,
    stops :: [SearchReqLocation],
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    riderPreferredOption :: SearchRequest.RiderPreferredOption,
    roundTrip :: Bool,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    merchant :: DM.Merchant,
    city :: City,
    device :: Maybe Text,
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    isReallocationEnabled :: Maybe Bool,
    multipleRoutes :: Maybe [Maps.RouteInfo],
    taggings :: Maybe Beckn.Taggings,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }

data SearchReqLocation = SearchReqLocation
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)


buildSearchReqLoc ::
  MonadFlow m =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  SearchReqLocation ->
  m Location.Location
buildSearchReqLoc merchantId merchantOperatingCityId SearchReqLocation {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  return
    Location.Location
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        address = address,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
