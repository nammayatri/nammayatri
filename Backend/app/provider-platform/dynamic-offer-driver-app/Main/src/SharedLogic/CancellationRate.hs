{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CancellationRate where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified Storage.Cac.TransporterConfig as CTC

data CancellationRateData = CancellationRateData
  { assignedCount :: Maybe Int,
    cancelledCount :: Maybe Int,
    cancellationRate :: Maybe Int,
    windowSize :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRideAssignedKey :: Text -> Text
mkRideAssignedKey driverId = "driver-offer:CR:assigned-dId:" <> driverId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey driverId = "driver-offer:CR:cancelled-dId:" <> driverId

getWindowSize :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Integer
getWindowSize mocId = do
  merchantConfig <- CTC.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  pure $ toInteger $ fromMaybe 7 merchantConfig.cancellationRateWindow

incrementCancelledCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementCancelledCount driverId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkRideCancelledKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

incrementAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Integer ->
  m ()
incrementAssignedCount driverId windowSize = Redis.withCrossAppRedis $ SWC.incrementWindowCount (mkRideAssignedKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getCancellationCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getCancellationCount windowSize driverId = Redis.withCrossAppRedis $ SWC.getCurrentWindowCount (mkRideCancelledKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getAssignedCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Integer ->
  Id DP.Person ->
  m Integer
getAssignedCount windowSize driverId = Redis.withCrossAppRedis $ SWC.getCurrentWindowCount (mkRideAssignedKey driverId.getId) (SWC.SlidingWindowOptions windowSize SWC.Days)

getCancellationRateData ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m CancellationRateData
getCancellationRateData mocId driverId = do
  merchantConfig <- CTC.findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  let windowSize = findWindowSize merchantConfig
      minimumRides = findMinimumRides merchantConfig
  (assignedCount, cancelledCount, cancellationRate) <- do
    assignedCount <- getAssignedCount windowSize driverId
    if (isJust merchantConfig.cancellationRateWindow) && (assignedCount > minimumRides)
      then do
        cancelledCount <- getCancellationCount windowSize driverId
        let cancellationRate = (cancelledCount * 100) `div` max 1 assignedCount
        pure (Just $ fromInteger assignedCount, Just $ fromInteger cancelledCount, Just $ fromInteger cancellationRate)
      else pure (Nothing, Nothing, Nothing)
  pure $ CancellationRateData assignedCount cancelledCount cancellationRate merchantConfig.cancellationRateWindow
  where
    findWindowSize merchantConfig = toInteger $ fromMaybe 7 merchantConfig.cancellationRateWindow
    findMinimumRides merchantConfig = toInteger $ fromMaybe 5 merchantConfig.cancellationRateCalculationThreshold
