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

type GetCollectionHistory =
  "collectionHistory"
    :> QueryParam "volunteerId" Text
    :> QueryParam "place" Text
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] CollectionList

-- from, to is on the basis of startTime
type GetAllDriverFeeHistory =
  "allFeeHistory"
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] [AllFees]

data AllFees = AllFees
  { status :: DriverFeeStatus,
    numRides :: Int,
    numDrivers :: Int,
    totalAmount :: HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CollectionListRes = CollectionListRes
  { totalCount :: Int,
    totalFeeCollected :: HighPrecMoney,
    totalRides :: Int,
    totalDrivers :: Int,
    collectionsTs :: [(HighPrecMoney, Maybe UTCTime)],
    numRidesTs :: [(Int, Maybe UTCTime)],
    paymentTs :: [(Int, Maybe UTCTime)]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CollectionList = CollectionList
  { onlineCollection :: CollectionListRes,
    offlineCollection :: CollectionListRes
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
    updatedAt :: UTCTime,
    collectedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
