{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search
  ( buildRentalSearchReq,
    buildOneWaySearchReq,
    buildRecurringSearchReq,
  )
where

import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Data.Set as S
import Data.Time.Calendar (DayOfWeek)
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Recurring as DRecurringSearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.SearchRequest as DSearchReq
import Environment
import EulerHS.Prelude hiding (state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Tools.Maps as Maps

buildOneWaySearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOneWaySearch.OneWaySearchRes ->
  Maybe Maps.RouteInfo ->
  m (BecknReq Search.SearchMessage)
buildOneWaySearchReq DOneWaySearch.OneWaySearchRes {..} routeInto =
  buildSearchReq origin (Just destination) searchId (OnlyTime now) routeInto city

buildRentalSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DRentalSearch.RentalSearchRes ->
  m (BecknReq Search.SearchMessage)
buildRentalSearchReq DRentalSearch.RentalSearchRes {..} =
  buildSearchReq origin Nothing searchId (OnlyTime startTime) Nothing city

buildRecurringSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DRecurringSearch.RecurringSearchRes ->
  m (BecknReq Search.SearchMessage)
buildRecurringSearchReq DRecurringSearch.RecurringSearchRes {..} =
  buildSearchReq origin (Just destination) searchId (TimeAndDays initialRideTime scheduleDays) Nothing ""

data SearchTimeParams
  = OnlyTime UTCTime
  | TimeAndDays UTCTime (S.Set DayOfWeek)

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSearchCommon.SearchReqLocation ->
  Maybe DSearchCommon.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  SearchTimeParams ->
  Maybe Maps.RouteInfo ->
  Text ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin mbDestination searchId searchTime mbRouteInfo city = do
  let transactionId = getId searchId
      messageId = transactionId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SEARCH messageId (Just transactionId) bapIDs.cabs bapURIs.cabs Nothing Nothing city
  let intent = mkIntent origin mbDestination searchTime
  let searchMessage = Search.SearchMessage intent mbRouteInfo
  pure $ BecknReq context searchMessage

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  Maybe DSearchCommon.SearchReqLocation ->
  SearchTimeParams ->
  Search.Intent
mkIntent origin mbDestination searchTime = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin,
            time = case searchTime of
              OnlyTime startTime ->
                Search.Time startTime Nothing
              TimeAndDays startTime days ->
                Search.Time startTime (Just days)
          }
      mkStopInfo destination =
        Search.StopInfo
          { location = mkLocation destination
          }
      mbEndLocation = mkStopInfo <$> mbDestination

      fulfillment =
        Search.FulfillmentInfo
          { start = startLocation,
            end = mbEndLocation
          }
  Search.Intent
    { ..
    }
  where
    mkLocation info =
      Search.Location
        { gps =
            Search.Gps
              { lat = info.gps.lat,
                lon = info.gps.lon
              },
          address = Nothing
        }
