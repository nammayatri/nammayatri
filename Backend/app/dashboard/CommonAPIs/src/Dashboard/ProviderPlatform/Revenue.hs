{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-}

module Dashboard.ProviderPlatform.Revenue
  ( module Dashboard.ProviderPlatform.Revenue,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Dashboard.ProviderPlatform.Driver (DriverFeeStatus)
import Data.Text
import GHC.Int
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (Summary)

type GetCashCollectionHistory =
  "cashCollectionHistory"
    :> MandatoryQueryParam "collectorId" Text
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] CashCollectionListRes

-- from, to is on the basis of startTime
type GetAllDriverFeeHistory =
  "allFeeHistory"
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] AllDriverFeeRes

data AllDriverFeeRes = AllDriverFeeRes
  { driversPaidOnline :: Int,
    driversPaidOffline :: Int,
    driversYetToPay :: Int,
    allFees :: [AllFees]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data AllFees = AllFees
  { status :: DriverFeeStatus,
    numRides :: Int,
    totalAmount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CashCollectionListRes = CashCollectionListRes
  { summary :: Summary,
    totalFeeCollected :: HighPrecMoney,
    collectedFees :: [DriverFeeAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverFeeAPIEntity = DriverFeeAPIEntity
  { id :: Id DriverFeeAPIEntity,
    merchantId :: Text,
    driverId :: Id Driver,
    fee :: HighPrecMoney,
    numRides :: Int,
    payBy :: UTCTime,
    totalEarnings :: Money,
    startTime :: UTCTime,
    endTime :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
