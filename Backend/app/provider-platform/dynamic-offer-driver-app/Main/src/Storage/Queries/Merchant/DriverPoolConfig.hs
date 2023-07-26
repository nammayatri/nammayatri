{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.DriverPoolConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.Merchant.DriverPoolConfig
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (Meters, MonadTime (getCurrentTime))
import Kernel.Types.Id
import Storage.Tabular.Merchant.DriverPoolConfig

create :: DriverPoolConfig -> SqlDB ()
create = Esq.create

findAllByMerchantId :: Transactionable m => Id Merchant -> m [DriverPoolConfig]
findAllByMerchantId merchantId =
  Esq.findAll $ do
    driverPoolConfig <- from $ table @DriverPoolConfigT
    where_ $
      driverPoolConfig ^. DriverPoolConfigMerchantId ==. val (toKey merchantId)
    orderBy [desc $ driverPoolConfig ^. DriverPoolConfigTripDistance]
    return driverPoolConfig

findByMerchantIdAndTripDistance :: Transactionable m => Id Merchant -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantIdAndTripDistance merchantId tripDistance =
  Esq.findOne $ do
    driverPoolConfig <- from $ table @DriverPoolConfigT
    where_ $
      driverPoolConfig ^. DriverPoolConfigTId ==. val (toKey (merchantId, tripDistance))
    return driverPoolConfig

update :: DriverPoolConfig -> SqlDB ()
update config = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverPoolConfigMinRadiusOfSearch =. val config.minRadiusOfSearch,
        DriverPoolConfigMaxRadiusOfSearch =. val config.maxRadiusOfSearch,
        DriverPoolConfigRadiusStepSize =. val config.radiusStepSize,
        DriverPoolConfigDriverPositionInfoExpiry =. val config.driverPositionInfoExpiry,
        DriverPoolConfigActualDistanceThreshold =. val config.actualDistanceThreshold,
        DriverPoolConfigMaxDriverQuotesRequired =. val config.maxDriverQuotesRequired,
        DriverPoolConfigDriverQuoteLimit =. val config.driverQuoteLimit,
        DriverPoolConfigDriverRequestCountLimit =. val config.driverRequestCountLimit,
        DriverPoolConfigDriverBatchSize =. val config.driverBatchSize,
        DriverPoolConfigMaxNumberOfBatches =. val config.maxNumberOfBatches,
        DriverPoolConfigMaxParallelSearchRequests =. val config.maxParallelSearchRequests,
        DriverPoolConfigPoolSortingType =. val config.poolSortingType,
        DriverPoolConfigSingleBatchProcessTime =. val config.singleBatchProcessTime,
        DriverPoolConfigUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverPoolConfigTId ==. val (toKey (config.merchantId, config.tripDistance))
