module Core.ACL.Search (buildRentalSearchReq, buildOneWaySearchReq) where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Search as Search
import Beckn.Types.Id
import qualified Domain.Action.UI.Search.Common as DSearchCommon
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.SearchRequest as DSearchReq
import EulerHS.Prelude hiding (state)
import ExternalAPI.Flow
import Utils.Common

buildOneWaySearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOneWaySearch.OneWaySearchRes ->
  m (BecknReq Search.SearchMessage)
buildOneWaySearchReq DOneWaySearch.OneWaySearchRes {..} = buildSearchReq origin (Just destination) searchId now

buildRentalSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DRentalSearch.RentalSearchRes ->
  m (BecknReq Search.SearchMessage)
buildRentalSearchReq DRentalSearch.RentalSearchRes {..} = buildSearchReq origin Nothing searchId startTime

buildSearchReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSearchCommon.SearchReqLocation ->
  Maybe DSearchCommon.SearchReqLocation ->
  Id DSearchReq.SearchRequest ->
  UTCTime ->
  m (BecknReq Search.SearchMessage)
buildSearchReq origin mbDestination searchId startTime = do
  let messageId = getId searchId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SEARCH messageId Nothing bapIDs.cabs bapURIs.cabs Nothing Nothing
  let intent = mkIntent origin mbDestination startTime
  pure $ BecknReq context $ Search.SearchMessage intent

mkIntent ::
  DSearchCommon.SearchReqLocation ->
  Maybe DSearchCommon.SearchReqLocation ->
  UTCTime ->
  Search.Intent
mkIntent origin mbDestination startTime = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation origin,
            time = Search.TimeTimestamp startTime
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
              }
        }
