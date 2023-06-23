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

module Storage.Tabular.DriverFee where

import Control.Monad
import Data.Time (UTCTime)
import qualified Domain.Types.DriverFee as Domain
import EulerHS.Prelude (Generic, Int, Text, ($))
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.DriverFeeStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverFeeT sql=driver_fee
      id Text
      shortId Text
      driverId PersonTId
      govtCharges Money
      platformFee Money
      cgst HighPrecMoney
      sgst HighPrecMoney
      payBy UTCTime
      startTime UTCTime
      endTime UTCTime
      numRides Int
      status Domain.DriverFeeStatus
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverFeeT where
  type DomainKey DriverFeeT = Id Domain.DriverFee
  fromKey (DriverFeeTKey _id) = Id _id
  toKey (Id id) = DriverFeeTKey id

instance FromTType DriverFeeT Domain.DriverFee where
  fromTType DriverFeeT {..} = do
    return $
      Domain.DriverFee
        { id = Id id,
          driverId = cast $ fromKey driverId,
          shortId = ShortId shortId,
          ..
        }

instance ToTType DriverFeeT Domain.DriverFee where
  toTType Domain.DriverFee {..} =
    DriverFeeT
      { driverId = toKey $ cast driverId,
        id = getId id,
        shortId = getShortId shortId,
        ..
      }
