{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.SearchRequest (postSearchRequestSearchrequests) where

import qualified API.Types.Dashboard.RideBooking.SearchRequest
import qualified API.Types.Dashboard.RideBooking.SearchRequest as SRType
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Clickhouse.SearchRequestForDriver as SCSRD
import Storage.Queries.SearchRequestExtra as SQSR

postSearchRequestSearchrequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SRType.SearchRequestsReq -> Environment.Flow SRType.SearchRequestsRes)
postSearchRequestSearchrequests _merchantShortId _opCity req = do
  reqs <- SCSRD.findByDriverId req.driverId req.fromDate req.toDate
  let requestIds = map (\(_, _, requestId, _, _) -> requestId) reqs
  sreqs <- SQSR.findSearchRequestById requestIds
  let populatedReqs = zip reqs sreqs
  let modifiedreq =
        map
          ( \(((_dt, srfdId, requestId, dist, dur), searchRequest)) ->
              SRType.SearchRequestOfDriver
                { createdAt = UTCTime (fromGregorian 2025 1 1) 0,
                  id = srfdId,
                  requestId = requestId,
                  tripEstimatedDistance = dist,
                  tripEstimatedDuration = dur,
                  fromLocation = searchRequest.fromLocation,
                  toLocation = searchRequest.toLocation
                }
          )
          populatedReqs
  return SRType.SearchRequestsRes {searchrequests = modifiedreq}
