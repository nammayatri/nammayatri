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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Consumer.AvailabilityTime.Storage.Tables where

import qualified Consumer.AvailabilityTime.Types as Domain
import Data.Time
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverAvailabilityT sql=driver_availability
      id Text
      driverId Text
      merchantId Text

      totalAvailableTime Int
      lastAvailableTime UTCTime
      bucketStartTime UTCTime
      bucketEndTime UTCTime
      createdAt UTCTime
      updatedAt UTCTime

      Unique (DriverAvailabilityBucketStartTime, DriverAvailabilityBucketEndTime, DriverAvailabilityDriverId, DriverAvailabilityMerchantId)
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverAvailabilityT where
  type DomainKey DriverAvailabilityT = Id Domain.DriverAvailability
  fromKey (DriverAvailabilityTKey _id) = Id _id
  toKey (Id id) = DriverAvailabilityTKey id

-- instance FromTType DriverAvailabilityT Domain.DriverAvailability where
--   fromTType DriverAvailabilityT {..} =
--     return
--       Domain.DriverAvailability
--         { id = Id id,
--           ..
--         }

-- instance ToTType DriverAvailabilityT Domain.DriverAvailability where
--   toTType Domain.DriverAvailability {..} =
--     DriverAvailabilityT
--       { id = getId id,
--         ..
--       }
