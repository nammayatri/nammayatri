{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Bus.Search (buildBusSearchReq) where

import Beckn.ACL.Common (mkLocation)
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified Data.Text as T
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Tools.Maps as Maps

buildBusSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DOneWaySearch.OneWaySearchRes ->
  m (BecknReq Search.SearchMessage)
buildBusSearchReq DOneWaySearch.OneWaySearchRes {..} =
  buildSearchReq
    origin
    destination
    searchId
    device
    (shortestRouteInfo >>= (.distance))
    (shortestRouteInfo >>= (.duration))
    customerLanguage
    disabilityTag
    merchant
    (getPoints shortestRouteInfo)
  where
    getPoints val = val >>= (\routeInfo -> Just routeInfo.points)

buildSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Maps.Language ->
  Maybe Text ->
  DM.Merchant ->
  Maybe [Maps.LatLong] ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin destination searchId _ distance duration customerLanguage disabilityTag merchant mbPoints = do
  let transactionId = getId searchId
      messageId = transactionId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  context <- buildContext Context.PUBLIC_TRANSPORT Context.SEARCH messageId (Just transactionId) merchant.bapId bapUrl Nothing Nothing merchant.city merchant.country False
  let intent = mkIntent origin destination customerLanguage disabilityTag distance duration mbPoints
  let searchMessage = Search.SearchMessage intent

  pure $ BecknReq context searchMessage

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  DSearchCommon.SearchReqLocation ->
  Maybe Maps.Language ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe [Maps.LatLong] ->
  Search.Intent
mkIntent origin destination _customerLanguage _disabilityTag _distance _duration _mbPoints = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin
          }
      endLocation =
        Search.StopInfo
          { location = mkLocation destination
          }

      fulfillment =
        Search.FulfillmentInfo
          { start = startLocation,
            end = endLocation,
            tags = Nothing,
            customer = Nothing
          }
  Search.Intent
    { ..
    }
