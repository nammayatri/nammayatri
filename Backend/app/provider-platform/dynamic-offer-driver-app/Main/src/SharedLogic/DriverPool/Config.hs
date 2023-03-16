{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Domain.Types.Merchant
import Domain.Types.Merchant.DriverPoolConfig
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getDriverPoolConfig :: (MonadFlow m, MonadReader r m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Meters -> m DriverPoolConfig
getDriverPoolConfig merchantId dist = do
  configs <- CDP.findAllByMerchantId merchantId
  let applicableConfig = find filterByDist configs
  case configs of
    [] -> throwError $ InvalidRequest "DriverPoolConfig not found"
    (config : _) -> pure $ maybe config identity applicableConfig
  where
    filterByDist cfg = dist >= cfg.tripDistance
