{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MerchantConfig
  ( updateCustomerFraudCounters,
    updateTotalRidesCounters,
    anyFraudDetected,
    mkCancellationKey,
  )
where

import Data.Foldable.Extra
import qualified Domain.Types.MerchantConfig as DMC
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Storage.CachedQueries.CacheConfig

mkCancellationKey :: Text -> Text
mkCancellationKey idtxt = "Customer:CancellationCount:" <> idtxt

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey idtxt = "Customer:TotalRidesCount:" <> idtxt

updateCustomerFraudCounters :: (HasCacheConfig r, HedisFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateCustomerFraudCounters riderId merchantConfigs = do
  mapM_ (\mc -> incrementCount mc.fraudBookingDetectionWindow) merchantConfigs
  where
    incrementCount = SWC.incrementWindowCount (mkCancellationKey riderId.getId)

updateTotalRidesCounters :: (HasCacheConfig r, HedisFlow m r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m ()
updateTotalRidesCounters riderId merchantConfigs = do
  mapM_ (\mc -> incrementCount mc.fraudBookingDetectionWindow) merchantConfigs
  where
    incrementCount = SWC.incrementWindowCount (mkTotalRidesKey riderId.getId)

anyFraudDetected :: (HedisFlow m r, HasCacheConfig r, MonadFlow m) => Id Person.Person -> [DMC.MerchantConfig] -> m Bool
anyFraudDetected riderId merchantConfigs = do
  isJust
    <$> findM
      ( \mc -> do
          cancelledBookingCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkCancellationKey riderId.getId) mc.fraudBookingDetectionWindow
          totalRidesCount :: Int <- sum . catMaybes <$> SWC.getCurrentWindowValues (mkTotalRidesKey riderId.getId) mc.fraudBookingDetectionWindow
          pure $ totalRidesCount <= mc.fraudBookingTotalCountThreshold && cancelledBookingCount >= mc.fraudBookingCancellationCountThreshold
      )
      merchantConfigs
