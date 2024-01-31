{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.DriverIntelligentPoolConfig where

import Data.Aeson
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.Text as Text
import Data.Time.Clock.POSIX
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant.MerchantOperatingCity as DTMM
import EulerHS.Prelude hiding (id)
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC

data DriverIntelligentPoolConfigD u = DriverIntelligentPoolConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id DTMM.MerchantOperatingCity,
    actualPickupDistanceWeightage :: Int,
    availabilityTimeWeightage :: Int,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: Int,
    acceptanceRatioWindowOption :: SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Int,
    cancellationAndRideFrequencyRatioWindowOption :: SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: Maybe Int,
    speedNormalizer :: Double, -- abnormally high speed
    driverSpeedWeightage :: Int,
    minLocationUpdates :: Int,
    locationUpdateSampleTime :: Minutes,
    defaultDriverSpeed :: Double,
    maxNumRides :: Int,
    numRidesWeightage :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

type DriverIntelligentPoolConfig = DriverIntelligentPoolConfigD 'Safe

instance FromJSON (DriverIntelligentPoolConfigD 'Unsafe)

instance ToJSON (DriverIntelligentPoolConfigD 'Unsafe)

data IntelligentFactors = AcceptanceRatio | CancellationRatio | AvailableTime | DriverSpeed | ActualPickupDistance | RideFrequency
  deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data IntelligentScores = IntelligentScores
  { acceptanceRatio :: Maybe Double,
    cancellationRatio :: Maybe Double,
    availableTime :: Maybe Double,
    driverSpeed :: Maybe Double,
    actualPickupDistanceScore :: Maybe Double,
    rideFrequency :: Maybe Double,
    rideRequestPopupDelayDuration :: Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read)

readWithInfo :: (Read a, Show a) => String -> a
readWithInfo s = case KP.readMaybe s of
  Just val -> val
  Nothing -> error . Text.pack $ "Failed to parse: " ++ s

jsonToDriverIntelligentPoolConfig :: Object -> (Parser DriverIntelligentPoolConfig)
jsonToDriverIntelligentPoolConfig v =
  DriverIntelligentPoolConfig
    <$> (Id <$> (v .: DAK.fromText (Text.pack "merchantId")))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "merchantOperatingCityId")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "actualPickupDistanceWeightage")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "availabilityTimeWeightage")))
    <*> ((readWithInfo :: (String -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "availabilityTimeWindowOption")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "acceptanceRatioWeightage")))
    <*> ((readWithInfo :: (String -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "acceptanceRatioWindowOption")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "cancellationRatioWeightage")))
    <*> ((readWithInfo :: (String -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "cancellationAndRideFrequencyRatioWindowOption")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "minQuotesToQualifyForIntelligentPool")))
    <*> ((readWithInfo :: (String -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "minQuotesToQualifyForIntelligentPoolWindowOption")))
    <*> ((v .: DAK.fromText (Text.pack "intelligentPoolPercentage")))
    <*> ((readWithInfo :: (String -> Double)) <$> (v .: DAK.fromText (Text.pack "speedNormalizer")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverSpeedWeightage")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "minLocationUpdates")))
    <*> ((readWithInfo :: (String -> Minutes)) <$> (v .: DAK.fromText (Text.pack "locationUpdateSampleTime")))
    <*> ((readWithInfo :: (String -> Double)) <$> (v .: DAK.fromText (Text.pack "defaultDriverSpeed")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "maxNumRides")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "numRidesWeightage")))
    <*> (pure (posixSecondsToUTCTime 0))
    <*> (pure (posixSecondsToUTCTime 0))
