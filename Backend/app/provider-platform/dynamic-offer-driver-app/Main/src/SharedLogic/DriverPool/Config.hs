{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Variant.Variant ->
  Meters ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId Nothing dist = do
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  getDefaultDriverPoolConfig configs dist
getDriverPoolConfig merchantOpCityId (Just vehicle) dist = do
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  let mbApplicableConfig = find (filterByDistAndDveh (Just vehicle) dist) configs
  case configs of
    [] -> throwError $ InvalidRequest "DriverPoolConfig not found"
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> return applicableConfig
        Nothing -> getDefaultDriverPoolConfig configs dist

filterByDistAndDveh :: Maybe Variant.Variant -> Meters -> DriverPoolConfig -> Bool
filterByDistAndDveh mbVehicle_ dist cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == mbVehicle_

getDefaultDriverPoolConfig :: (EsqDBFlow m r) => [DriverPoolConfig] -> Meters -> m DriverPoolConfig
getDefaultDriverPoolConfig configs dist = do
  find (filterByDistAndDveh Nothing dist) configs
    & fromMaybeM (InvalidRequest "DriverPool default config not found")
