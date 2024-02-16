{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Coins.CoinHistory where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Coin as DCoins hiding (CoinStatus)
import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (id, state)
import Kernel.Types.Id
import qualified Lib.DriverCoins.Types as DCT
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data CoinHistory = CoinHistory
  { id :: Id CoinHistory,
    driverId :: Text,
    merchantId :: Text,
    merchantOptCityId :: Text,
    eventFunction :: DCT.DriverCoinsFunctionType,
    coins :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    expirationAt :: Maybe UTCTime,
    coinsUsed :: Int,
    status :: CoinStatus,
    bulkUploadTitle :: Maybe DCoins.Translations
  }
  deriving (Generic, Read, Show, Eq, FromJSON, ToJSON)

data CoinStatus = Used | Remaining deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''CoinStatus)
