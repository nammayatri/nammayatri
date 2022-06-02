{-# LANGUAGE OverloadedLabels #-}

module Core.ACL.Metro.Search (buildSearchReq) where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Migration.API.Search as Search
import qualified Beckn.Types.Core.Migration.Gps as Gps
import qualified Beckn.Types.Core.Migration.Intent as Intent
import qualified Beckn.Types.Core.Migration.Location as Location
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Control.Lens ((?~))
import qualified Domain.Action.UI.Search.OneWay as DSearch
import EulerHS.Prelude hiding (state)
import ExternalAPI.Flow
import Product.MetroOffer (buildContextMetro)
import qualified Types.API.Search as API
import Utils.Common

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSearch.DSearchReq ->
  m (BecknReq Search.SearchIntent)
buildSearchReq req@DSearch.DSearchReq {..} = do
  let txnId = getId searchId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildContextMetro Context.SEARCH txnId bapIDs.metro bapURIs.metro
  let intent = mkIntent req
  pure $ BecknReq context $ Search.SearchIntent intent

mkIntent :: DSearch.DSearchReq -> Intent.Intent
mkIntent req = do
  let from = stopToLoc req.origin
  let to = stopToLoc req.destination
  Intent.emptyIntent
    & #fulfillment
      ?~ ( Intent.emptyFulFillmentInfo
             & #start
               ?~ Intent.LocationAndTime
                 { location = Just from,
                   time = Nothing
                 }
             & #end
               ?~ Intent.LocationAndTime
                 { location = Just to,
                   time = Nothing
                 }
         )
  where
    stopToLoc API.SearchReqLocation {gps} = do
      let gps' = Gps.Gps gps.lat gps.lon
      Location.emptyLocation & #gps ?~ gps'
