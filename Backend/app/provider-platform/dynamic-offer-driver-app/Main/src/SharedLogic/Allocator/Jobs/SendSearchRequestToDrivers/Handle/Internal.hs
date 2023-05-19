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
    ifSearchRequestInvalid,
    module Reexport,
  )
where

import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.SearchTry as DST
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as Reexport
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers as Reexport
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

ifSearchRequestInvalid ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Id SearchTry ->
  m Bool
ifSearchRequestInvalid searchTryId = do
  (validTill, status) <- QST.getSearchTryStatusAndValidTill searchTryId >>= fromMaybeM (SearchTryDoesNotExist searchTryId.getId)
  now <- getCurrentTime
  pure $ status == DST.CANCELLED || validTill <= now

isRideAlreadyAssigned ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Id SearchTry ->
  m Bool
isRideAlreadyAssigned searchTryId = isJust <$> QB.findBySTId searchTryId

isReceivedMaxDriverQuotes ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  DriverPoolConfig ->
  Id SearchTry ->
  m Bool
isReceivedMaxDriverQuotes driverPoolCfg searchTryId = do
  totalQuotesRecieved <- length <$> QDQ.findAllBySTId searchTryId
  pure (totalQuotesRecieved >= driverPoolCfg.maxDriverQuotesRequired)

getRescheduleTime ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Seconds ->
  m UTCTime
getRescheduleTime singleBatchProcessTime = do
  now <- getCurrentTime
  return $ fromIntegral singleBatchProcessTime `addUTCTime` now

setBatchDurationLock ::
  ( MonadFlow m,
    HedisFlow m r
  ) =>
  Id SearchTry ->
  Seconds ->
  m (Maybe UTCTime)
setBatchDurationLock searchRequestId singleBatchProcessTime = do
  now <- getCurrentTime
  res <- Hedis.setNxExpire (getId searchRequestId) (fromIntegral singleBatchProcessTime) now
  if not res
    then do Hedis.get (getId searchRequestId)
    else return Nothing

createRescheduleTime ::
  MonadReader r m =>
  Seconds ->
  UTCTime ->
  m UTCTime
createRescheduleTime singleBatchProcessTime lastProcTime = do
  return $ fromIntegral singleBatchProcessTime `addUTCTime` lastProcTime
