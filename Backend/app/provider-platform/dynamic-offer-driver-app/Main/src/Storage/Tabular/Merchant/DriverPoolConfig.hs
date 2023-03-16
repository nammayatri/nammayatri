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

module Storage.Tabular.Merchant.DriverPoolConfig where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.DriverPoolConfig as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters, Seconds)
import Kernel.Types.Id
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config (PoolSortingType)
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "PoolSortingType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverPoolConfigT sql=driver_pool_config
      merchantId MerchantTId
      minRadiusOfSearch Meters
      maxRadiusOfSearch Meters
      radiusStepSize Meters
      driverPositionInfoExpiry Seconds Maybe
      actualDistanceThreshold Meters Maybe
      maxDriverQuotesRequired Int
      maxParallelSearchRequests Int
      driverQuoteLimit Int
      driverRequestCountLimit Int
      driverBatchSize Int
      minDriverBatchSize Int
      maxNumberOfBatches Int
      poolSortingType PoolSortingType
      singleBatchProcessTime Seconds
      tripDistance Meters
      createdAt UTCTime
      updatedAt UTCTime
      Primary merchantId tripDistance
      deriving Generic
    |]

instance TEntityKey DriverPoolConfigT where
  type DomainKey DriverPoolConfigT = (Id DM.Merchant, Meters)
  fromKey (DriverPoolConfigTKey _id tripDistance) = (fromKey _id, tripDistance)
  toKey (id, tripDistance) = DriverPoolConfigTKey (toKey id) tripDistance

instance FromTType DriverPoolConfigT Domain.DriverPoolConfig where
  fromTType DriverPoolConfigT {..} = do
    return $
      Domain.DriverPoolConfig
        { merchantId = fromKey merchantId,
          ..
        }

instance ToTType DriverPoolConfigT Domain.DriverPoolConfig where
  toTType Domain.DriverPoolConfig {..} =
    DriverPoolConfigT
      { merchantId = toKey merchantId,
        ..
      }
