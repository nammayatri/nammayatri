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
    findAllByAreaIncludingDefaultAndCityAndVariant,
  )
where

import Data.List (nub)
import qualified Domain.Types.MerchantOperatingCity as DMOC
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
  Hedis.safeGet (makeAreaIncludingDefaultAndCityAndVariantKey mbArea merchantOperatingCityId vehicleVariant) >>= \case
    Just a -> pure a
    Nothing ->
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp (makeAreaIncludingDefaultAndCityAndVariantKey mbArea merchantOperatingCityId vehicleVariant) dataToBeCached expTime
      )
        /=<< Queries.findAllByAreasCityAndVariant (nub $ SpecialLocation.Default : toList mbArea) merchantOperatingCityId vehicleVariant

makeAreaIncludingDefaultAndCityAndVariantKey ::
  Maybe SpecialLocation.Area ->
  Id DMOC.MerchantOperatingCity ->
  DVehicleVariant.VehicleVariant ->
  Text
makeAreaIncludingDefaultAndCityAndVariantKey mbArea merchantOperatingCityId vehicleVariant = do
  let areaKey = case mbArea of
        Just area -> if area == SpecialLocation.Default then "" else ":Area-" <> show area
        Nothing -> ""
  "driverOfferCachedQueries:VendorSplitDetails:"
    <> areaKey
    <> ":IncludingDefault:MerchantOperatingCityId-"
    <> Kernel.Types.Id.getId merchantOperatingCityId
    <> ":VehicleVariant-"
    <> show vehicleVariant
