module Domain.Action.Dashboard.RideBooking.SearchRequest (postSearchRequestSearchrequests, getSearchRequestList) where

import qualified API.Types.Dashboard.RideBooking.SearchRequest
import qualified API.Types.Dashboard.RideBooking.SearchRequest as SRType
import qualified Data.Map as Map
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Storage.ClickhouseV2.ClickhouseValue (DateTime (..))
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Clickhouse.SearchRequestForDriver as SCSRD
import Storage.Queries.SearchRequestExtra as SQSR

postSearchRequestSearchrequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SRType.SearchRequestsReq -> Environment.Flow SRType.SearchRequestsRes)
postSearchRequestSearchrequests _merchantShortId _opCity req = do
  reqs <- SCSRD.findByDriverId req.driverId req.fromDate req.toDate req.mbLimit req.mbOffset
  let requestIds = map (\(_, _, requestId, _, _) -> requestId) reqs
  sreqs <- SQSR.findSearchRequestById requestIds
  let sreqMap = Map.fromList [(sr.id, sr) | sr <- sreqs]
  let filteredReqs = filter (\(_, _, requestId, _, _) -> Map.member requestId sreqMap) reqs
  let modifiedreq =
        map
          ( \(dt, srfdId, requestId, dist, dur) ->
              let searchRequest = sreqMap Map.! requestId
               in SRType.SearchRequestOfDriver
                    { createdAt = getDateTime dt,
                      id = srfdId,
                      requestId = requestId,
                      tripEstimatedDistance = dist,
                      tripEstimatedDuration = dur,
                      toLocation = searchRequest.toLocation,
                      fromLocation = Just searchRequest.fromLocation
                    }
          )
          filteredReqs
  return SRType.SearchRequestsRes {searchrequests = modifiedreq}

getSearchRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.Flow API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
getSearchRequestList _merchantShortId _opCity driverId fromDate toDate mbLimit mbOffset = do
  reqs <- SCSRD.findByDriverId driverId fromDate toDate mbLimit mbOffset
  let requestIds = map (\(_, _, requestId, _, _) -> requestId) reqs
  sreqs <- SQSR.findSearchRequestById requestIds
  let sreqMap = Map.fromList [(sr.id, sr) | sr <- sreqs]
  let filteredReqs = filter (\(_, _, requestId, _, _) -> Map.member requestId sreqMap) reqs
  let modifiedreq =
        map
          ( \(dt, srfdId, requestId, dist, dur) ->
              let searchRequest = sreqMap Map.! requestId
               in SRType.SearchRequestOfDriver
                    { createdAt = getDateTime dt,
                      id = srfdId,
                      requestId = requestId,
                      tripEstimatedDistance = dist,
                      tripEstimatedDuration = dur,
                      toLocation = searchRequest.toLocation,
                      fromLocation = Just searchRequest.fromLocation
                    }
          )
          filteredReqs
  return SRType.SearchRequestsRes {searchrequests = modifiedreq}
