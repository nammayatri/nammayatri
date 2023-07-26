{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.DriverIntelligentPoolConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Storage.Tabular.Merchant.DriverIntelligentPoolConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe DriverIntelligentPoolConfig)
findByMerchantId merchantId =
  Esq.findOne $ do
    config <- from $ table @DriverIntelligentPoolConfigT
    where_ $
      config ^. DriverIntelligentPoolConfigMerchantId ==. val (toKey merchantId)
    return config

update :: DriverIntelligentPoolConfig -> SqlDB ()
update config = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverIntelligentPoolConfigAvailabilityTimeWeightage =. val config.availabilityTimeWeightage,
        DriverIntelligentPoolConfigAvailabilityTimeWindowOption =. val config.availabilityTimeWindowOption,
        DriverIntelligentPoolConfigAcceptanceRatioWeightage =. val config.acceptanceRatioWeightage,
        DriverIntelligentPoolConfigAcceptanceRatioWindowOption =. val config.acceptanceRatioWindowOption,
        DriverIntelligentPoolConfigCancellationRatioWeightage =. val config.cancellationRatioWeightage,
        DriverIntelligentPoolConfigCancellationRatioWindowOption =. val config.cancellationRatioWindowOption,
        DriverIntelligentPoolConfigMinQuotesToQualifyForIntelligentPool =. val config.minQuotesToQualifyForIntelligentPool,
        DriverIntelligentPoolConfigMinQuotesToQualifyForIntelligentPoolWindowOption =. val config.minQuotesToQualifyForIntelligentPoolWindowOption,
        DriverIntelligentPoolConfigIntelligentPoolPercentage =. val config.intelligentPoolPercentage,
        DriverIntelligentPoolConfigSpeedNormalizer =. val config.speedNormalizer,
        DriverIntelligentPoolConfigDriverSpeedWeightage =. val config.driverSpeedWeightage,
        DriverIntelligentPoolConfigMinLocationUpdates =. val config.minLocationUpdates,
        DriverIntelligentPoolConfigLocationUpdateSampleTime =. val config.locationUpdateSampleTime,
        DriverIntelligentPoolConfigDefaultDriverSpeed =. val config.defaultDriverSpeed,
        DriverIntelligentPoolConfigUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverIntelligentPoolConfigMerchantId ==. val (toKey config.merchantId)
