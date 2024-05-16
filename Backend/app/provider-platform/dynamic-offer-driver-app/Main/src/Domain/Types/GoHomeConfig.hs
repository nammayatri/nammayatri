{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.GoHomeConfig where

-- import Data.Aeson
import Data.Text as Text
import Data.Time (UTCTime)
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as KP
import Kernel.Types.Common (Distance, Seconds)
import Kernel.Types.Id

--------------------------------------------------------------------------------------

data Subscriber

-- FIXME fix CAC types parsing
data GoHomeConfig = GoHomeConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    enableGoHome :: Bool,
    startCnt :: Int,
    destRadius :: Distance,
    activeTime :: Int,
    updateHomeLocationAfterSec :: Int,
    cancellationCnt :: Int,
    numHomeLocations :: Int,
    goHomeFromLocationRadius :: Distance,
    goHomeWayPointRadius :: Distance,
    numDriversForDirCheck :: Int,
    goHomeBatchDelay :: Seconds,
    ignoreWaypointsTill :: Distance,
    addStartWaypointAt :: Distance,
    newLocAllowedRadius :: Distance,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

readWithInfo :: (Read a, Show a) => String -> a
readWithInfo s = case KP.readMaybe s of
  Just val -> val
  Nothing -> error . Text.pack $ "Failed to parse: " ++ s
