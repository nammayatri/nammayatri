{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.Overlay
  ( create,
    findAllByMerchantOpCityId,
    clearMerchantIdPNKeyLangaugeUdf,
    clearCache,
    findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.MerchantOperatingCity
import Domain.Types.Overlay
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Overlay as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Overlay -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [Overlay]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
    Just a -> return $ fmap (coerce @(OverlayD 'Unsafe) @Overlay) a
    Nothing -> cacheOverlayForCity id /=<< Queries.findAllByMerchantOpCityId id

cacheOverlayForCity :: CacheFlow m r => Id MerchantOperatingCity -> [Overlay] -> m ()
cacheOverlayForCity merchantOperatingCityId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (fmap (coerce @Overlay @(OverlayD 'Unsafe)) cfg) expTime

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:Overlay:MerchantOperatingCityId-" <> id.getId

findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Maybe DVC.VehicleCategory -> m (Maybe Overlay)
findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory id pnKey language udf1 vehicleCategory =
  Hedis.safeGet (makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1 vehicleCategory) >>= \case
    Just a -> return . Just $ coerce @(OverlayD 'Unsafe) @Overlay a
    Nothing -> do
      res <- Queries.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory id pnKey language udf1 vehicleCategory
      case res of
        Just a -> do
          cacheOverlay a
          return $ Just a
        Nothing -> case vehicleCategory of
          Nothing -> return Nothing
          _ -> findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory id pnKey language udf1 Nothing

cacheOverlay :: CacheFlow m r => Overlay -> m ()
cacheOverlay pn = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdPNKeyLangaugeUdf pn.merchantOperatingCityId pn.overlayKey pn.language pn.udf1 pn.vehicleCategory
  Hedis.setExp idKey (coerce @Overlay @(OverlayD 'Unsafe) pn) expTime

makeMerchantIdPNKeyLangaugeUdf :: Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Maybe DVC.VehicleCategory -> Text
makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1 mbVehicleCategory = "CachedQueries:Overlay:MerchantOpCityId-" <> id.getId <> ":PNKey-" <> show pnKey <> ":ln-" <> show language <> ":udf1-" <> show udf1 <> maybe "" ((":VehCat-" <>) . show) mbVehicleCategory

------------------------------------------------------

clearMerchantIdPNKeyLangaugeUdf :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Maybe DVC.VehicleCategory -> m ()
clearMerchantIdPNKeyLangaugeUdf merchantOpCityId overlayKey language udf1 vehicleCategory = Hedis.del $ makeMerchantIdPNKeyLangaugeUdf merchantOpCityId overlayKey language udf1 vehicleCategory

clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = do
  Hedis.del (makeMerchantOpCityIdKey merchantOperatingCityId)
