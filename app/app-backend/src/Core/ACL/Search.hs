module Core.ACL.Search (buildRentalSearchReq, buildOneWaySearchReq) where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Search as Search
import Beckn.Types.Id
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (state)
import ExternalAPI.Flow
import qualified Types.API.Search as API
import Utils.Common

buildOneWaySearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOneWaySearch.DSearchReq ->
  m (BecknReq Search.SearchMessage)
buildOneWaySearchReq DOneWaySearch.DSearchReq {..} = buildSearchReq origin (Just destination) searchId now

buildRentalSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DRentalSearch.DSearchReq ->
  m (BecknReq Search.SearchMessage)
buildRentalSearchReq DRentalSearch.DSearchReq {..} = buildSearchReq origin Nothing searchId now

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  API.SearchReqLocation ->
  Maybe API.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  UTCTime ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin mbDestination searchId now = do
  let txnId = getId searchId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SEARCH txnId bapIDs.cabs bapURIs.cabs Nothing Nothing
  let intent = mkIntent origin mbDestination now
  pure $ BecknReq context $ Search.SearchMessage intent

mkIntent ::
  API.SearchReqLocation ->
  Maybe API.SearchReqLocation ->
  UTCTime ->
  Search.Intent
mkIntent origin mbDestination now = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin,
            time = Search.Time now
          }
      mkStopInfo destination =
        Search.StopInfo
          { location = mkLocation destination
          }
      mbEndLocation = mkStopInfo <$> mbDestination

      fulfillment =
        Search.FulFillmentInfo
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
          address = do
            let API.SearchReqAddress {..} = info.address
            Search.Address
              { area_code = areaCode,
                ..
              }
        }
