{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.VendorSplitDetails
  ( create,
    createMany,
    makeCityAndPlanKey,
    makeDefaultKey,
    makeAreaOnlyKey,
    makeRowAreaKey,
    findAllByAreaIncludingDefaultAndCityAndVariant,
    findAllByCityAndPlan,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.VehicleVariant as DVehicleVariant
import qualified Domain.Types.VendorSplitDetails as DVSD
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SpecialLocation
import qualified Storage.Queries.VendorSplitDetails as Queries

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DVSD.VendorSplitDetails -> m ()
create = Queries.create

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [DVSD.VendorSplitDetails] -> m ()
createMany = Queries.createMany

findAllByAreaIncludingDefaultAndCityAndVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe SpecialLocation.Area ->
  Id DMOC.MerchantOperatingCity ->
  DVehicleVariant.VehicleVariant ->
  m [DVSD.VendorSplitDetails]
findAllByAreaIncludingDefaultAndCityAndVariant mbArea merchantOperatingCityId vehicleVariant = do
  -- Default rows and area-specific rows are cached under separate keys and merged here, so changing a
  -- Default row only invalidates makeDefaultKey (never the per-area keys). This avoids the earlier
  -- fold-in, where one Default row was baked into every queried area key and could go stale unnoticed.
  defaultRows <- getOrCacheRows (makeDefaultKey merchantOperatingCityId vehicleVariant) [SpecialLocation.Default]
  areaRows <- case mbArea of
    Just area | area /= SpecialLocation.Default -> getOrCacheRows (makeAreaOnlyKey area merchantOperatingCityId vehicleVariant) [area]
    _ -> pure []
  pure (defaultRows <> areaRows)
  where
    getOrCacheRows key areas =
      Hedis.safeGet key >>= \case
        Just a -> pure a
        Nothing ->
          ( \dataToBeCached -> do
              expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
              Hedis.setExp key dataToBeCached expTime
          )
            /=<< Queries.findAllByAreasCityAndVariant areas merchantOperatingCityId vehicleVariant

makeDefaultKey ::
  Id DMOC.MerchantOperatingCity ->
  DVehicleVariant.VehicleVariant ->
  Text
makeDefaultKey merchantOperatingCityId vehicleVariant =
  "driverOfferCachedQueries:VendorSplitDetails:Default:MerchantOperatingCityId-"
    <> Kernel.Types.Id.getId merchantOperatingCityId
    <> ":VehicleVariant-"
    <> show vehicleVariant

makeAreaOnlyKey ::
  SpecialLocation.Area ->
  Id DMOC.MerchantOperatingCity ->
  DVehicleVariant.VehicleVariant ->
  Text
makeAreaOnlyKey area merchantOperatingCityId vehicleVariant =
  "driverOfferCachedQueries:VendorSplitDetails:AreaOnly:Area-"
    <> show area
    <> ":MerchantOperatingCityId-"
    <> Kernel.Types.Id.getId merchantOperatingCityId
    <> ":VehicleVariant-"
    <> show vehicleVariant

-- | The cache key a given row lives in, routing Default rows to makeDefaultKey and everything else to
-- its own area key. Used by the dashboard clear paths so invalidation matches exactly one read key.
makeRowAreaKey :: DVSD.VendorSplitDetails -> Text
makeRowAreaKey vendorSplit =
  if vendorSplit.area == SpecialLocation.Default
    then makeDefaultKey vendorSplit.merchantOperatingCityId vendorSplit.vehicleVariant
    else makeAreaOnlyKey vendorSplit.area vendorSplit.merchantOperatingCityId vendorSplit.vehicleVariant

findAllByCityAndPlan ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id DPlan.Plan ->
  m [DVSD.VendorSplitDetails]
findAllByCityAndPlan merchantOperatingCityId planId = do
  Hedis.safeGet (makeCityAndPlanKey merchantOperatingCityId planId) >>= \case
    Just a -> pure a
    Nothing ->
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp (makeCityAndPlanKey merchantOperatingCityId planId) dataToBeCached expTime
      )
        /=<< Queries.findAllByCityAndPlan merchantOperatingCityId (Just (Kernel.Types.Id.getId planId))

makeCityAndPlanKey ::
  Id DMOC.MerchantOperatingCity ->
  Id DPlan.Plan ->
  Text
makeCityAndPlanKey merchantOperatingCityId planId =
  "driverOfferCachedQueries:VendorSplitDetails:CityAndPlan:MerchantOperatingCityId-"
    <> Kernel.Types.Id.getId merchantOperatingCityId
    <> ":PlanId-"
    <> Kernel.Types.Id.getId planId
