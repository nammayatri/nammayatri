{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.CachedQueries.HotSpotConfig where

import Domain.Types.HotSpotConfig
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.HotSpotConfig as Queries

findConfigByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe HotSpotConfig)
findConfigByMerchantId merchantId =
  Hedis.safeGet (makeIdKey merchantId) >>= \case
    Just a -> return a
    Nothing -> do
      let hotSpotId :: Id HotSpotConfig = Id (getId merchantId)
      flip whenJust (cacheFindConfigByMerchantId merchantId) /=<< Queries.findConfigByMerchantId hotSpotId

cacheFindConfigByMerchantId :: CacheFlow m r => Id Merchant -> HotSpotConfig -> m ()
cacheFindConfigByMerchantId merchantId hotSpotConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeIdKey merchantId
  Hedis.setExp key hotSpotConfig expTime

makeIdKey :: Id Merchant -> Text
makeIdKey merchantId = "CachedQueries:HotSpotConfig:-" <> show merchantId
