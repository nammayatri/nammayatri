{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DuplicateRecordFields #-}

module SharedLogic.NySubscription where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Search as DSearch
import Domain.Types.Location as Location
import qualified Domain.Types.NyRegularSubscription as NyRegularSubscription
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Interface as MapsInterface
import Kernel.Types.App (HasFlowEnv)
import Kernel.Types.Id
import Kernel.Utils.Common (type (:::))
import SharedLogic.Search (OneWaySearchReq (..), SearchReq (..), SearchReqLocation (..))
import TransactionLogs.Types

triggerSubscriptionSearch ::
  ( DSearch.SearchRequestFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
  ) =>
  NyRegularSubscription.NyRegularSubscription ->
  m (Id DSearchReq.SearchRequest)
triggerSubscriptionSearch subscription = do
  let oneWaySearchReq =
        OneWaySearchReq
          { origin = toSearchReqLocation subscription.pickupLocation,
            destination = Just $ toSearchReqLocation subscription.dropoffLocation,
            stops = Nothing,
            isSourceManuallyMoved = Nothing,
            isDestinationManuallyMoved = Nothing,
            isSpecialLocation = Nothing,
            startTime = Nothing,
            isReallocationEnabled = Nothing,
            fareParametersInRateCard = Nothing,
            quotesUnifiedFlow = Nothing,
            sessionToken = Nothing,
            placeNameSource = Nothing,
            driverIdentifier = Nothing,
            isMeterRideSearch = Nothing,
            recentLocationId = Nothing,
            platformType = Nothing,
            isReserveRide = Just True,
            subscriptionId = Just subscription.id
          }

  searchResp <-
    DSearch.search
      subscription.userId
      (OneWaySearch oneWaySearchReq)
      Nothing -- backendConfigVersion
      Nothing -- clientBundleVersion
      Nothing -- clientConfigVersion
      Nothing -- clientDevice
      Nothing -- clientId
      Nothing -- clientSdkVersion
      False -- isDashboardRequest
      Nothing -- journeySearchData
      False -- isMeterRide
  return searchResp.searchRequest.id

toSearchReqLocation :: Location.Location -> SearchReqLocation
toSearchReqLocation loc =
  SearchReqLocation
    { gps = LatLong loc.lat loc.lon,
      address = loc.address
    }
