{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant.DriverIntelligentPoolConfig where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.DriverIntelligentPoolConfig as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Types.SlidingWindowCounters (PeriodType)
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "PeriodType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverIntelligentPoolConfigT sql=driver_intelligent_pool_config
      merchantId MerchantTId
      availabilityTimeWeightage Int
      availabilityTimeWindowOption SWC.SlidingWindowOptions
      acceptanceRatioWeightage Int
      acceptanceRatioWindowOption SWC.SlidingWindowOptions
      cancellationRatioWeightage Int
      cancellationRatioWindowOption SWC.SlidingWindowOptions
      minQuotesToQualifyForIntelligentPool Int
      minQuotesToQualifyForIntelligentPoolWindowOption SWC.SlidingWindowOptions
      intelligentPoolPercentage Int Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary merchantId
      deriving Generic
    |]

instance TEntityKey DriverIntelligentPoolConfigT where
  type DomainKey DriverIntelligentPoolConfigT = Id DM.Merchant
  fromKey (DriverIntelligentPoolConfigTKey _id) = fromKey _id
  toKey id = DriverIntelligentPoolConfigTKey $ toKey id

instance FromTType DriverIntelligentPoolConfigT Domain.DriverIntelligentPoolConfig where
  fromTType DriverIntelligentPoolConfigT {..} = do
    return $
      Domain.DriverIntelligentPoolConfig
        { merchantId = fromKey merchantId,
          ..
        }

instance ToTType DriverIntelligentPoolConfigT Domain.DriverIntelligentPoolConfig where
  toTType Domain.DriverIntelligentPoolConfig {..} =
    DriverIntelligentPoolConfigT
      { merchantId = toKey merchantId,
        ..
      }
