module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal
  ( isRideAlreadyAssigned,
    getRescheduleTime,
    isReceivedMaxDriverQuotes,
    setBatchDurationLock,
    createRescheduleTime,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis (HedisFlow)
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.SearchRequest
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as Reexport
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers as Reexport
import SharedLogic.DriverPool (HasDriverPoolConfig)
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.DriverQuote as QDQ

isRideAlreadyAssigned ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Id SearchRequest ->
  m Bool
isRideAlreadyAssigned searchReqId = isJust <$> QB.findBySearchReq searchReqId

isReceivedMaxDriverQuotes ::
  ( HasCacheConfig r,
    HasDriverPoolConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Id SearchRequest ->
  m Bool
isReceivedMaxDriverQuotes searchReqId = do
  totalQuotesRecieved <- length <$> QDQ.findAllByRequestId searchReqId
  maxDriverQuotesRequired <- asks (.driverPoolCfg.maxDriverQuotesRequired)
  pure (totalQuotesRecieved >= maxDriverQuotesRequired)

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
