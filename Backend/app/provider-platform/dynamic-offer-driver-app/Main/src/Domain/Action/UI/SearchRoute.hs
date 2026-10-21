module Domain.Action.UI.SearchRoute (postSearchRoute) where

import qualified API.Types.UI.SearchRoute
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RideRoute as RI
import qualified Domain.Types.SearchTry as DST
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Ride (searchRequestKey)
import qualified Storage.Queries.QueriesExtra.SearchRequestLite as QSRLite
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Maps as Maps

-- Called when the ride request popup is shown. Returns both legs in one call: the pickup leg
-- (driver -> pickup) from the maps provider, and the trip leg (pickup -> drop) from the route
-- cached in redis at search time. Everything that can fail is checked before the paid maps call.
postSearchRoute ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id DST.SearchTry ->
    Kernel.External.Maps.GetRoutesReq ->
    Environment.Flow API.Types.UI.SearchRoute.SearchRouteResp
  )
postSearchRoute (mbPersonId, merchantId, merchantOpCityId) searchTryId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  searchTry <- QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  now <- getCurrentTime
  when (searchTry.validTill < now) $ throwError SearchRequestExpired
  searchRequest <- QSRLite.findByIdLite searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  tripRoute :: RI.RouteInfo <-
    Redis.runInMultiCloudRedisMaybeResult (Redis.safeGet $ searchRequestKey searchRequest.transactionId)
      >>= fromMaybeM (RouteNotFound searchTryId.getId)
  pickupRoute <- Maps.getPickupRoutes merchantId merchantOpCityId (Just personId.getId) req
  pure $ API.Types.UI.SearchRoute.SearchRouteResp {pickupRoute, tripRoute}
