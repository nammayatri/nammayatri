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
-- import Data.Time.Clock.POSIX

import Data.Aeson as A
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.Text as Text
import qualified Data.Text.Encoding as DTE
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

readWithInfo :: (Read a, Show a) => Value -> a
readWithInfo s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: " ++ Text.unpack str
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: " ++ show scientific
  _ -> error $ "Not able to parse value" <> show s

readWithInfo' :: (Read a, Show a) => Value -> Maybe a
readWithInfo' s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> Just val
    Nothing -> Nothing
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> Just val
    Nothing -> Nothing
  _ -> error $ "Not able to parse value" <> show s

valueToType :: FromJSON a => Value -> a
valueToType value = case value of
  String str -> case A.decode (BL.fromStrict (DTE.encodeUtf8 (replaceSingleQuotes str))) of
    Just val -> val
    Nothing -> error $ "Not able to parse value" <> show value
  _ -> error $ "Not able to parse value" <> show value

replaceSingleQuotes :: Text -> Text
replaceSingleQuotes = Text.replace "'" "\""

jsonToDriverIntelligentPoolConfig :: Object -> (Parser DriverIntelligentPoolConfig)
jsonToDriverIntelligentPoolConfig v =
  DriverIntelligentPoolConfig
    <$> (Id <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:merchantId")))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:merchantOperatingCityId")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:actualPickupDistanceWeightage")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:availabilityTimeWeightage")))
    <*> ((valueToType :: (Value -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:availabilityTimeWindowOption")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:acceptanceRatioWeightage")))
    <*> ((valueToType :: (Value -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:acceptanceRatioWindowOption")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:cancellationRatioWeightage")))
    <*> ((valueToType :: (Value -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:cancellationAndRideFrequencyRatioWindowOption")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:minQuotesToQualifyForIntelligentPool")))
    <*> ((valueToType :: (Value -> SWC.SlidingWindowOptions)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:minQuotesToQualifyForIntelligentPoolWindowOption")))
    <*> ((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:intelligentPoolPercentage")))
    <*> ((readWithInfo :: (Value -> Double)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:speedNormalizer")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:driverSpeedWeightage")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:minLocationUpdates")))
    <*> ((readWithInfo :: (Value -> Minutes)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:locationUpdateSampleTime")))
    <*> ((readWithInfo :: (Value -> Double)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:defaultDriverSpeed")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:maxNumRides")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:numRidesWeightage")))
    <*> ((readWithInfo :: (Value -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:createdAt")))
    <*> ((readWithInfo :: (Value -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "driverIntelligentPoolConfig:updatedAt")))
