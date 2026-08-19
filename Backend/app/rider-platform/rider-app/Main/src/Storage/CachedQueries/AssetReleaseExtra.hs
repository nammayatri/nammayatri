{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.AssetReleaseExtra where

import Domain.Types.AssetRelease
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AssetRelease as Queries

findLatest ::
  (CacheFlow m r, EsqDBFlow m r) =>
  AssetType ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m (Maybe AssetRelease)
findLatest assetType merchantId merchantOperatingCityId = do
  Hedis.safeGet (makeLatestKey assetType merchantId merchantOperatingCityId) >>= \case
    Just release -> return $ Just release
    Nothing -> do
      mbRelease <- Queries.findLatestByAssetTypeAndCity (Just 1) Nothing assetType merchantId merchantOperatingCityId <&> listToMaybe
      whenJust mbRelease cacheLatest
      return mbRelease

findAllLatest ::
  (CacheFlow m r, EsqDBFlow m r) =>
  [AssetType] ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [AssetRelease]
findAllLatest assetTypes merchantId merchantOperatingCityId =
  catMaybes <$> mapM (\assetType -> findLatest assetType merchantId merchantOperatingCityId) assetTypes

cacheLatest :: (CacheFlow m r) => AssetRelease -> m ()
cacheLatest release = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp
    (makeLatestKey release.assetType release.merchantId release.merchantOperatingCityId)
    release
    expTime

makeLatestKey :: AssetType -> Id Merchant -> Id MerchantOperatingCity -> Text
makeLatestKey assetType merchantId merchantOperatingCityId =
  "rider-app:CachedQueries:AssetRelease:Latest-" <> show assetType <> "-" <> merchantId.getId <> "-" <> merchantOperatingCityId.getId

clearCache :: Hedis.HedisFlow m r => AssetType -> Id Merchant -> Id MerchantOperatingCity -> m ()
clearCache assetType merchantId merchantOperatingCityId =
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeLatestKey assetType merchantId merchantOperatingCityId)
