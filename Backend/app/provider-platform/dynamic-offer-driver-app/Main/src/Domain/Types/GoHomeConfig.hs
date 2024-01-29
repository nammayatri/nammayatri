{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.GoHomeConfig where

import Data.Aeson
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as KP
import Kernel.Types.Common (Meters, Seconds)
import Kernel.Types.Id

--------------------------------------------------------------------------------------

data Subscriber

data GoHomeConfig = GoHomeConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    enableGoHome :: Bool,
    startCnt :: Int,
    destRadiusMeters :: Int,
    activeTime :: Int,
    updateHomeLocationAfterSec :: Int,
    cancellationCnt :: Int,
    numHomeLocations :: Int,
    goHomeFromLocationRadius :: Meters,
    goHomeWayPointRadius :: Meters,
    numDriversForDirCheck :: Int,
    goHomeBatchDelay :: Seconds,
    ignoreWaypointsTill :: Meters,
    addStartWaypointAt :: Meters,
    newLocAllowedRadius :: Meters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

jsonToGoHomeConfig :: Object -> (Parser GoHomeConfig)
jsonToGoHomeConfig v =
  GoHomeConfig
    <$> (Id <$> (v .: DAK.fromText (Text.pack "merchantId")))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "merchantOperatingCityId")))
    <*> ((KP.read :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "enableGoHome")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "startCnt")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "destRadiusMeters")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "activeTime")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "updateHomeLocationAfterSec")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "cancellationCnt")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "numHomeLocations")))
    <*> ((KP.read :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "goHomeFromLocationRadius")))
    <*> ((KP.read :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "goHomeWayPointRadius")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "numDriversForDirCheck")))
    <*> ((KP.read :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "goHomeBatchDelay")))
    <*> ((KP.read :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "ignoreWaypointsTill")))
    <*> ((KP.read :: (String -> Meters)) <$> v .: DAK.fromText (Text.pack "addStartWaypointAt")) -- Think about something
    <*> ((KP.read :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "newLocAllowedRadius")))
    <*> (pure (posixSecondsToUTCTime 0))
    <*> (pure (posixSecondsToUTCTime 0))
