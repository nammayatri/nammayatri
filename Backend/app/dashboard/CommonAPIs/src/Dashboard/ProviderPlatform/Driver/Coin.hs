{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Driver.Coin
  ( module Dashboard.ProviderPlatform.Driver.Coin,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum, mkBeamInstancesForJSON)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Servant hiding (Summary, throwError)

type BulkUploadCoinsAPI =
  "bulkUploadCoins"
    :> ReqBody '[JSON] BulkUploadCoinsReq
    :> Post '[JSON] APISuccess

type CoinHistoryAPI =
  "coinHistory"
    :> Capture "driverId" (Id Driver)
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] CoinHistoryRes

data BulkUploadCoinsReq = BulkUploadCoinsReq
  { driverIdListWithCoins :: [DriverIdListWithCoins],
    bulkUploadTitle :: Translations,
    expirationTime :: Maybe Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverIdListWithCoins = DriverIdListWithCoins
  { driverId :: Text,
    coins :: Int
  }
  deriving (Generic, Read, Eq, Show, FromJSON, ToJSON, Ord, ToSchema)

data Translations = Translations
  { en :: Text,
    bn :: Text,
    hi :: Text,
    ml :: Text,
    ta :: Text,
    te :: Text,
    kn :: Text,
    fr :: Text
  }
  deriving (Generic, Read, Eq, Show, FromJSON, ToJSON, Ord, ToSchema)

data CoinHistoryRes = CoinHistoryRes
  { coinBalance :: Int,
    coinEarned :: Int,
    coinUsed :: Int,
    coinExpired :: Int,
    coinEarnHistory :: [CoinEarnHistoryItem],
    coinBurnHistory :: [CoinBurnHistoryItem]
  }
  deriving (Generic, Read, Eq, Show, FromJSON, ToJSON, Ord, ToSchema)

data CoinEarnHistoryItem = CoinEarnHistoryItem
  { coins :: Int,
    eventFunction :: DriverCoinsFunctionType,
    createdAt :: UTCTime,
    expirationAt :: Maybe UTCTime,
    coinsUsed :: Int,
    status :: CoinStatus,
    bulkUploadTitle :: Maybe Translations
  }
  deriving (Generic, Read, Eq, Show, FromJSON, ToJSON, Ord, ToSchema)

data CoinBurnHistoryItem = CoinBurnHistoryItem
  { numCoins :: Int,
    cash :: HighPrecMoney,
    title :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Read, Eq, Show, FromJSON, ToJSON, Ord, ToSchema)

data CoinStatus = Used | Remaining deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data DriverCoinsFunctionType
  = OneOrTwoStarRating
  | RideCompleted
  | FiveStarRating
  | BookingCancellation
  | CustomerReferral
  | DriverReferral
  | EightPlusRidesInOneDay
  | PurpleRideCompleted
  | LeaderBoardTopFiveHundred
  | TrainingCompleted
  | BulkUploadFunction
  deriving (Show, Eq, Read, Generic, FromJSON, ToSchema, ToJSON, Ord, Typeable)

$(mkBeamInstancesForEnum ''DriverCoinsFunctionType)

$(mkBeamInstancesForEnum ''CoinStatus)

$(mkBeamInstancesForJSON ''Translations)
