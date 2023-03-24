{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverScore
  (driverScoreEventHandler,
   OnDriverAcceptingSearchRequest(..),
   OnNewRideAssigned(..),
   module Reexport,
  )
where

import Lib.DriverScore.Types as Reexport
import Kernel.Prelude
import Kernel.Types.Id
import qualified SharedLogic.DriverPool as DP
import Kernel.Utils.Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Kernel.Storage.Hedis as Redis
import Data.Typeable (typeRep)
import qualified Domain.Types.SearchRequest as SR
import qualified Domain.Types.SearchRequestForDriver as SRD

driverScoreEventHandler :: forall a m r. (DP.HasDriverPoolConfig r, Log m, Monad m, EventPayload a, Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r, Show a, Typeable a) => a -> m ()
driverScoreEventHandler payload = fork "DRIVER_SCORE_EVENT_HANDLER" do
  now <- getCurrentTime
  let event = Event payload now
  logTagInfo (show . typeRep $ Proxy @a) (show event)
  handleEvent event.payload

data OnDriverAcceptingSearchRequest = OnDriverAcceptingSearchRequest
  { merchantId :: Id DM.Merchant
  , driverId :: Id DP.Person
  , searchReqId :: Id SR.SearchRequest
  , restDriverIds :: [Id DP.Person]
  , response :: SRD.SearchRequestForDriverResponse
  } deriving (Show, Typeable)

instance EventPayload OnDriverAcceptingSearchRequest where
  handleEvent OnDriverAcceptingSearchRequest {..} = do
    DP.decrementTotalQuotesCount merchantId (cast driverId) searchReqId
    case response of
      SRD.Accept -> do
        DP.incrementQuoteAcceptedCount merchantId driverId
        forM_ restDriverIds $ \restDriverId -> do
          DP.decrementTotalQuotesCount merchantId (cast restDriverId) searchReqId
          DP.removeSearchReqIdFromMap merchantId restDriverId searchReqId
      SRD.Reject -> pure ()
      SRD.Pulled -> pure ()

data OnNewRideAssigned = OnNewRideAssigned
  { merchantId :: Id DM.Merchant
  , driverId :: Id DP.Person
  } deriving (Show, Typeable)

instance EventPayload OnNewRideAssigned where
  handleEvent OnNewRideAssigned {..} = DP.incrementTotalRidesCount merchantId driverId
