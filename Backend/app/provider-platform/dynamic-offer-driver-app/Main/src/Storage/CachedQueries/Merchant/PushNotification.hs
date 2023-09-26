{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.PushNotification
  ( findByMerchantIdPNKeyLangaugeUdf,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.PushNotification
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.PushNotification as Queries

findByMerchantIdPNKeyLangaugeUdf :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> PushNotificationKey -> Language -> Maybe Text -> m (Maybe PushNotification)
findByMerchantIdPNKeyLangaugeUdf id pnKey language udf1 =
  Hedis.get (makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1) >>= \case
    Just a -> return . Just $ coerce @(PushNotificationD 'Unsafe) @PushNotification a
    Nothing -> flip whenJust cachePushNotification /=<< Queries.findByMerchantIdPNKeyLangaugeUdf id pnKey language udf1

cachePushNotification :: CacheFlow m r => PushNotification -> m ()
cachePushNotification pn = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdPNKeyLangaugeUdf pn.merchantId pn.pushNotificationKey pn.language pn.udf1
  Hedis.setExp idKey (coerce @PushNotification @(PushNotificationD 'Unsafe) pn) expTime

makeMerchantIdPNKeyLangaugeUdf :: Id Merchant -> PushNotificationKey -> Language -> Maybe Text -> Text
makeMerchantIdPNKeyLangaugeUdf id pnKey language udf1 = "CachedQueries:PushNotification:MerchantId-" <> id.getId <> ":PNKey-" <> show pnKey <> ":ln-" <> show language <> ":udf1-" <> show udf1
