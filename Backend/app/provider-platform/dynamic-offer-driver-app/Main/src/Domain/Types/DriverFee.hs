{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DriverFee where

import Data.Time (UTCTime)
import Domain.Types.Person (Driver)
import EulerHS.Prelude (Eq, Generic, Int, Read, Show)
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id

data DriverFee = DriverFee
  { id :: Id DriverFee,
    shortId :: ShortId DriverFee,
    driverId :: Id Driver,
    govtCharges :: Money,
    platformFee :: Money,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney,
    numRides :: Int,
    payBy :: UTCTime,
    totalEarnings :: Money,
    startTime :: UTCTime,
    endTime :: UTCTime,
    status :: DriverFeeStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data DriverFeeStatus = ONGOING | PAYMENT_PENDING | PAYMENT_OVERDUE | CLEARED | EXEMPTED deriving (Read, Show, Eq)
