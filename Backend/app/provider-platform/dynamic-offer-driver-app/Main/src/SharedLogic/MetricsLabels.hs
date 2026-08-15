{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MetricsLabels (getMetricsLabels, getCityLabel) where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

-- | Human-readable Prometheus label values for the BPP funnel counters:
-- merchant shortId and operating city name, falling back to the raw ids when
-- the cached lookups miss so a metric is never silently dropped.
-- Prefer in-scope domain objects (merchant.shortId.getShortId / show city) over
-- these lookups; use 'getCityLabel' when only the merchant object is in scope.
getMetricsLabels :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m (Text, Text)
getMetricsLabels merchantId merchantOpCityId = do
  mbMerchant <- CQM.findById merchantId
  cityLabel <- getCityLabel merchantOpCityId
  pure (maybe merchantId.getId (.shortId.getShortId) mbMerchant, cityLabel)

getCityLabel :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m Text
getCityLabel merchantOpCityId = do
  mbCity <- CQMOC.findById merchantOpCityId
  pure $ maybe merchantOpCityId.getId (show . (.city)) mbCity
