{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import qualified Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
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

getSearchDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Meters ->
  m DriverPoolConfig
getSearchDriverPoolConfig merchantOpCityId mbDist = do
  let distance = fromMaybe 0 mbDist
      vehicle = Nothing
      tripCategory = "All"
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  findDriverPoolConfig configs vehicle tripCategory distance

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Variant.Variant ->
  DTC.TripCategory ->
  Maybe Meters ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId vehicle tripCategory mbDist = do
  let distance = fromMaybe 0 mbDist
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  let mbApplicableConfig = find (filterByDistAndDveh (Just vehicle) (show tripCategory) distance) configs
  case configs of
    [] -> throwError $ InvalidRequest $ "DriverPool Configs not found for MerchantOperatingCity: " <> merchantOpCityId.getId
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> return applicableConfig
        Nothing -> do
          let alternativeConfigs = find (filterByDistAndDveh (Just vehicle) "All" distance) configs
          case alternativeConfigs of
            Just cfg -> return cfg
            Nothing -> findDriverPoolConfig configs Nothing "All" distance

filterByDistAndDveh :: Maybe Variant.Variant -> Text -> Meters -> DriverPoolConfig -> Bool
filterByDistAndDveh vehicle tripCategory dist cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == vehicle && cfg.tripCategory == tripCategory

findDriverPoolConfig :: (EsqDBFlow m r) => [DriverPoolConfig] -> Maybe Variant.Variant -> Text -> Meters -> m DriverPoolConfig
findDriverPoolConfig configs vehicle tripCategory dist = do
  find (filterByDistAndDveh vehicle tripCategory dist) configs
    & fromMaybeM (InvalidRequest $ "DriverPool Config not found: " <> show vehicle <> show tripCategory <> show dist)
