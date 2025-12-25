{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantState where

import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantState (MerchantState)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantState as Queries

findByMerchantIdAndState :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Context.IndianState -> m (Maybe MerchantState)
findByMerchantIdAndState merchantId state =
  Hedis.safeGet (makeMerchantIdAndStateKey merchantId state) >>= \case
    Just a -> return a
    Nothing -> cacheMerchantIdAndState merchantId state /=<< Queries.findByMerchantIdAndState merchantId state

cacheMerchantIdAndState :: CacheFlow m r => Id Merchant -> Context.IndianState -> Maybe MerchantState -> m ()
cacheMerchantIdAndState merchantId state merchantState = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdAndStateKey = makeMerchantIdAndStateKey merchantId state
  Hedis.setExp merchantIdAndStateKey merchantState expTime

makeMerchantIdAndStateKey :: Id Merchant -> Context.IndianState -> Text
makeMerchantIdAndStateKey merchantId state = "CachedQueries:MerchantState:MerchantId-" <> merchantId.getId <> ":State-" <> show state
