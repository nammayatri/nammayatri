{-# LANGUAGE OverloadedLabels #-}

module Core.ACL.Metro.Search (buildSearchReq) where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Metro.API.Search as Search
import qualified Beckn.Types.Core.Metro.Search as Search
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Types.TimeRFC339
import Control.Lens ((?~))
import qualified Domain.Action.UI.Search.OneWay as DSearch
import EulerHS.Prelude hiding (state)
import ExternalAPI.Flow
import Utils.Common

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
        core_version = "0.9.1",
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
