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
  ( findByMerchantIdPNKeyLangaugeUdf,
    clearMerchantIdPNKeyLangaugeUdf,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.Overlay
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.Overlay as Queries

findByMerchantIdPNKeyLangaugeUdf :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Text -> Language -> Maybe Text -> m (Maybe Overlay)
findByMerchantIdPNKeyLangaugeUdf id pnKey language udf1 =
  Hedis.get (makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1) >>= \case
    Just a -> return . Just $ coerce @(OverlayD 'Unsafe) @Overlay a
    Nothing -> flip whenJust cacheOverlay /=<< Queries.findByMerchantIdPNKeyLangaugeUdf id pnKey language udf1

cacheOverlay :: CacheFlow m r => Overlay -> m ()
cacheOverlay pn = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdPNKeyLangaugeUdf pn.merchantId pn.overlayKey pn.language pn.udf1
  Hedis.setExp idKey (coerce @Overlay @(OverlayD 'Unsafe) pn) expTime

makeMerchantIdPNKeyLangaugeUdf :: Id Merchant -> Text -> Language -> Maybe Text -> Text
makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1 = "CachedQueries:Overlay:MerchantId-" <> id.getId <> ":PNKey-" <> show pnKey <> ":ln-" <> show language <> ":udf1-" <> show udf1

------------------------------------------------------

clearMerchantIdPNKeyLangaugeUdf :: Hedis.HedisFlow m r => Id Merchant -> Text -> Language -> Maybe Text -> m ()
clearMerchantIdPNKeyLangaugeUdf merchantId overlayKey language udf1 = Hedis.del $ makeMerchantIdPNKeyLangaugeUdf merchantId overlayKey language udf1
