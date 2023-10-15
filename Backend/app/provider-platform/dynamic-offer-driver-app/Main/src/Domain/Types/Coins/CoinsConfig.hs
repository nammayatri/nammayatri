{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Coins.CoinsConfig where

import EulerHS.Prelude hiding (id, state)
import Kernel.Types.Id
import qualified Lib.DriverCoins.Types as DCT

data CoinsConfig = CoinsConfig
  { id :: Id CoinsConfig,
    eventFunction :: DCT.DriverCoinsFunctionType,
    eventName :: Text,
    merchantId :: Text,
    merchantOptCityId :: Text,
    coins :: Int,
    expirationAt :: Maybe Int,
    active :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
