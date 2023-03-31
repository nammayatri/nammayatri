{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverScore
  ( driverScoreEventHandler,
  )
where

import qualified Domain.Types.SearchRequestForDriver as SRD
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (cast)
import Kernel.Utils.Common (EsqDBFlow, Forkable (fork), logDebug)
import qualified Lib.DriverScore.Types as DST
import qualified SharedLogic.DriverPool as DP
import Storage.CachedQueries.CacheConfig (CacheFlow)

driverScoreEventHandler :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => DST.DriverRideRequeset -> m ()
driverScoreEventHandler payload = fork "DRIVER_SCORE_EVENT_HANDLER" do
  logDebug $ "driverScoreEventHandler with payload: " <> show payload
  eventPayloadHandler payload

eventPayloadHandler :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => DST.DriverRideRequeset -> m ()
eventPayloadHandler DST.OnDriverAcceptingSearchRequest {..} = do
  DP.decrementTotalQuotesCount merchantId (cast driverId) searchReqId
  case response of
    SRD.Accept -> do
      DP.incrementQuoteAcceptedCount merchantId driverId
      forM_ restDriverIds $ \restDriverId -> do
        DP.decrementTotalQuotesCount merchantId (cast restDriverId) searchReqId
        DP.removeSearchReqIdFromMap merchantId restDriverId searchReqId
    SRD.Reject -> pure ()
    SRD.Pulled -> pure ()
eventPayloadHandler DST.OnNewRideAssigned {..} = DP.incrementTotalRidesCount merchantId driverId
eventPayloadHandler DST.OnNewSearchRequestForDrivers {..} =
  forM_ driverPool $ \dPoolRes -> DP.incrementTotalQuotesCount searchReq.providerId (cast dPoolRes.driverPoolResult.driverId) searchReq validTill batchProcessTime
eventPayloadHandler DST.OnDriverCancellation {..} = DP.incrementCancellationCount merchantId driverId
