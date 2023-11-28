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
  ( findByMerchantOpCityIdPNKeyLangaugeUdf,
    clearMerchantIdPNKeyLangaugeUdf,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant.ConfigMapping (ConfigMapping)
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.Overlay
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.ConfigMapping as CMQ
import qualified Storage.Queries.Merchant.Overlay as Queries

--CMTODO: Handle Dashboard calls
findByMerchantOpCityIdPNKeyLangaugeUdf :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> Meters -> Maybe Variant -> m (Maybe Overlay)
findByMerchantOpCityIdPNKeyLangaugeUdf id pnKey language udf1 distance mbvt = do
  currTime <- getLocalCurrentTime 19800
  cmId <- CMQ.getConfigMapId id distance mbvt currTime "overlay" >>= fromMaybeM (InternalError $ "ConfigMapping not found for Overlay : mocid, distance, mbvt, currTime" <> show id <> "," <> show distance <> ", " <> show mbvt <> ", " <> show currTime)
  Hedis.safeGet (makeConfigMapIdPNKeyLangaugeUdf cmId pnKey language udf1) >>= \case
    Just a -> return . Just $ coerce @(OverlayD 'Unsafe) @Overlay a
    Nothing -> flip whenJust cacheOverlay /=<< Queries.findByConfigMapIdPNKeyLangaugeUdf cmId pnKey language udf1

cacheOverlay :: CacheFlow m r => Overlay -> m ()
cacheOverlay pn = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeConfigMapIdPNKeyLangaugeUdf pn.configMapId pn.overlayKey pn.language pn.udf1
  Hedis.setExp idKey (coerce @Overlay @(OverlayD 'Unsafe) pn) expTime

makeConfigMapIdPNKeyLangaugeUdf :: Id ConfigMapping -> Text -> Language -> Maybe Text -> Text
makeConfigMapIdPNKeyLangaugeUdf cmId pnKey language udf1 = "CachedQueries:Overlay:ConfigMapId-" <> id.getId <> ":PNKey-" <> show pnKey <> ":ln-" <> show language <> ":udf1-" <> show udf1

------------------------------------------------------

clearMerchantIdPNKeyLangaugeUdf :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> Text -> Language -> Maybe Text -> m ()
clearMerchantIdPNKeyLangaugeUdf merchantOpCityId overlayKey language udf1 = Hedis.del $ makeMerchantIdPNKeyLangaugeUdf merchantOpCityId overlayKey language udf1
