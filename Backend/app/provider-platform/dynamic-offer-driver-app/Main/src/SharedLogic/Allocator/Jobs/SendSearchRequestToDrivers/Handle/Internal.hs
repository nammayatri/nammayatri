{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal
  ( isRideAlreadyAssigned,
    getRescheduleTime,
    isReceivedMaxDriverQuotes,
    setBatchDurationLock,
    createRescheduleTime,
    ifSearchRequestIsCancelled,
    ifSearchRequestIsExpired,
    module Reexport,
  )
where

import Domain.Types.SearchRequest as SR
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (SearchRequestError (SearchRequestDoesNotExist))
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as Reexport
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers as Reexport
import SharedLogic.DriverPool (DriverPoolConfig)
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.SearchRequest as SR

ifSearchRequestIsCancelled ::
  forall m r.
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Id SearchRequest ->
  m Bool
ifSearchRequestIsCancelled searchReqId = do
  searchReqStatus <- SR.getStatus searchReqId (Proxy @m) >>= fromMaybeM (SearchRequestDoesNotExist searchReqId.getId)
  pure $ searchReqStatus == SR.CANCELLED

ifSearchRequestIsExpired ::
  forall m r.
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Id SearchRequest ->
  m Bool
ifSearchRequestIsExpired searchReqId = do
  searchReqValidTill <- SR.getValidTill searchReqId (Proxy @m) >>= fromMaybeM (SearchRequestDoesNotExist searchReqId.getId)
  now <- getCurrentTime
  pure $ searchReqValidTill <= now

isRideAlreadyAssigned ::
  forall m r.
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Id SearchRequest ->
  m Bool
isRideAlreadyAssigned searchReqId = isJust <$> QB.findBySearchReq searchReqId (Proxy @m)

isReceivedMaxDriverQuotes ::
  forall m r.
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  DriverPoolConfig ->
  Id SearchRequest ->
  m Bool
isReceivedMaxDriverQuotes driverPoolCfg searchReqId = do
  totalQuotesRecieved <- length <$> QDQ.findAllByRequestId searchReqId (Proxy @m)
  pure (totalQuotesRecieved >= driverPoolCfg.maxDriverQuotesRequired)

getRescheduleTime ::
  ( HasCacheConfig r,
    HasSendSearchRequestJobConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  m UTCTime
getRescheduleTime = do
  now <- getCurrentTime
  singleBatchProcessTime <- fromIntegral <$> asks (.sendSearchRequestJobCfg.singleBatchProcessTime)
  return $ singleBatchProcessTime `addUTCTime` now

setBatchDurationLock ::
  ( HasSendSearchRequestJobConfig r,
    MonadFlow m,
    HedisFlow m r
  ) =>
  Id SearchRequest ->
  m (Maybe UTCTime)
setBatchDurationLock searchRequestId = do
  now <- getCurrentTime
  singleBatchProcessTime <- fromIntegral <$> asks (.sendSearchRequestJobCfg.singleBatchProcessTime)
  res <- Hedis.setNxExpire (getId searchRequestId) singleBatchProcessTime now
  if not res
    then do Hedis.get (getId searchRequestId)
    else return Nothing

createRescheduleTime ::
  ( HasSendSearchRequestJobConfig r,
    MonadReader r m
  ) =>
  UTCTime ->
  m UTCTime
createRescheduleTime lastProcTime = do
  singleBatchProcessTime <- fromIntegral <$> asks (.sendSearchRequestJobCfg.singleBatchProcessTime)
  return $ singleBatchProcessTime `addUTCTime` lastProcTime
