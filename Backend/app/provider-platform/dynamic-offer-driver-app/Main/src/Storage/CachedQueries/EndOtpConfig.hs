{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.EndOtpConfig where

import Domain.Types.EndOtpConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Queries.EndOtpConfig as Queries

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => EndOtpConfig -> m ()
create = Queries.create

-- tripCategory/tripMode are the Text-encoded forms of TripCategory (e.g. "EasyBooking", "RideOtp"),
-- matching the pattern in Domain.Types.Trip's Show/Read instances.
getEndOtpConfigFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Text -> m (Maybe EndOtpConfig)
getEndOtpConfigFromDB merchantOperatingCityId tripCategory tripMode = do
  Hedis.safeGet (makeEndOtpConfigKey merchantOperatingCityId tripCategory tripMode) >>= \case
    Just cfg -> return (Just cfg)
    Nothing -> do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Queries.findByMerchantOpCityIdAndTripCategoryAndTripMode merchantOperatingCityId tripCategory tripMode >>= \case
        Just cfg -> do
          Hedis.setExp (makeEndOtpConfigKey merchantOperatingCityId tripCategory tripMode) cfg expTime
          return (Just cfg)
        Nothing -> return Nothing

findByMerchantOpCityIdAndTripCategoryAndTripMode :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Text -> m (Maybe EndOtpConfig)
findByMerchantOpCityIdAndTripCategoryAndTripMode = getEndOtpConfigFromDB

makeEndOtpConfigKey :: Id MerchantOperatingCity -> Text -> Text -> Text
makeEndOtpConfigKey merchantOperatingCityId tripCategory tripMode =
  "driver-offer:CachedQueries:EndOtpConfig:MerchantOpCityId-" <> merchantOperatingCityId.getId <> ":TripCategory-" <> tripCategory <> ":TripMode-" <> tripMode
