 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Metro.Search (buildSearchReq) where

import qualified Beckn.Types.Core.Metro.API.Search as Search
import qualified Beckn.Types.Core.Metro.Search as Search
import Control.Lens ((?~))
import qualified Domain.Action.UI.Search.OneWay as DSearch
import Environment
import EulerHS.Prelude hiding (state)
import Kernel.Types.Beckn.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSearch.OneWaySearchRes ->
  m (BecknReq Search.SearchIntent)
buildSearchReq req@DSearch.OneWaySearchRes {..} = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  let messageId = getId searchId
  context <- buildContextMetro Context.SEARCH messageId bapIDs.metro bapURIs.metro
  let intent = mkIntent req
  pure $ BecknReq context $ Search.SearchIntent intent

buildContextMetro ::
  (MonadTime m, MonadGuid m, MonadThrow m) =>
  Action ->
  Text ->
  Text ->
  BaseUrl ->
  m Context
buildContextMetro action message_id bapId bapUri = do
  timestamp <- UTCTimeRFC3339 <$> getCurrentTime
  return
    Context
      { domain = METRO,
        country = "IND",
        city = "Kochi",
        core_version = "0.9.3",
        bap_id = bapId,
        bap_uri = bapUri,
        bpp_id = Nothing,
        bpp_uri = Nothing,
        transaction_id = Nothing,
        ..
      }

mkIntent :: DSearch.OneWaySearchRes -> Search.Intent
mkIntent req = do
  let from = stopToLoc req.origin
  let to = stopToLoc req.destination
  Search.emptyIntent
    & #fulfillment
      ?~ ( Search.emptyFulFillmentInfo
             & #start
               ?~ Search.LocationAndTime
                 { location = Just from,
                   time = Nothing
                 }
             & #end
               ?~ Search.LocationAndTime
                 { location = Just to,
                   time = Nothing
                 }
         )
  where
    stopToLoc DSearch.SearchReqLocation {gps} = do
      let gps' = Search.Gps gps.lat gps.lon
      Search.emptyLocation & #gps ?~ gps'
