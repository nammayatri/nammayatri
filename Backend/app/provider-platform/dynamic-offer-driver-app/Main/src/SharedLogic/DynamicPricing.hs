{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DynamicPricing where

import Data.Aeson
import Data.Default.Class
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FarePolicy.DriverExtraFeeBounds as DDriverExtraFeeBounds
import qualified Domain.Types.ServiceTierType as DServiceTierType
import qualified Domain.Types.VehicleCategory as DVC
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common

mkSupplyDemandRatioKeyWithGeohash :: Text -> Maybe DVC.VehicleCategory -> Text
mkSupplyDemandRatioKeyWithGeohash geohash vehicleCategory = "S_D_ratio_geohash" <> geohash <> "_vehicleCategory_" <> show vehicleCategory

mkActualQARKeyWithGeohash :: Text -> Maybe DVC.VehicleCategory -> Text
mkActualQARKeyWithGeohash geohash vehicleCategory = "A_QAR_geohash" <> geohash <> "_vehicleCategory_" <> show vehicleCategory

mkActualQARKeyWithGeohashAndDistanceBin :: Text -> Text -> Maybe DVC.VehicleCategory -> Text
mkActualQARKeyWithGeohashAndDistanceBin geohash distanceBin vehicleCategory = "A_QAR_geohash" <> geohash <> "_distanceBin_" <> distanceBin <> "_vehicleCategory_" <> show vehicleCategory

mkActualQARKeyWithCity :: Text -> Maybe DVC.VehicleCategory -> Text
mkActualQARKeyWithCity city vehicleCategory = "A_QAR_city" <> city <> "_vehicleCategory_" <> show vehicleCategory

mkActualQARKeyWithGeohashPast :: Text -> Maybe DVC.VehicleCategory -> Text
mkActualQARKeyWithGeohashPast geohash vehicleCategory = "A_QAR_geohash_past" <> geohash <> "_vehicleCategory_" <> show vehicleCategory

mkActualQARKeyWithGeohashAndDistanceBinPast :: Text -> Text -> Maybe DVC.VehicleCategory -> Text
mkActualQARKeyWithGeohashAndDistanceBinPast geohash distanceBin vehicleCategory = "A_QAR_geohash_past" <> geohash <> "_distanceBin_" <> distanceBin <> "_vehicleCategory_" <> show vehicleCategory

mkActualQARKeyWithCityPast :: Text -> Maybe DVC.VehicleCategory -> Text
mkActualQARKeyWithCityPast city vehicleCategory = "A_QAR_city_past" <> city <> "_vehicleCategory_" <> show vehicleCategory

mkCongestionKeyWithGeohash :: Text -> Maybe DVC.VehicleCategory -> Text
mkCongestionKeyWithGeohash geohash vehicleCategory = "Congestion_geohash" <> geohash <> "_vehicleCategory_" <> show vehicleCategory

mkCongestionKeyWithGeohashAndDistanceBin :: Text -> Text -> Maybe DVC.VehicleCategory -> Text
mkCongestionKeyWithGeohashAndDistanceBin geohash distanceBin vehicleCategory = "Congestion_geohash" <> geohash <> "_distanceBin_" <> distanceBin <> "_vehicleCategory_" <> show vehicleCategory

mkCongestionKeyWithCity :: Text -> Maybe DVC.VehicleCategory -> Text
mkCongestionKeyWithCity city vehicleCategory = "Congestion_city" <> city <> "_vehicleCategory_" <> show vehicleCategory

mkCongestionKeyWithGeohashPast :: Text -> Maybe DVC.VehicleCategory -> Text
mkCongestionKeyWithGeohashPast geohash vehicleCategory = "Congestion_geohash_past" <> geohash <> "_vehicleCategory_" <> show vehicleCategory

mkCongestionKeyWithGeohashAndDistanceBinPast :: Text -> Text -> Maybe DVC.VehicleCategory -> Text
mkCongestionKeyWithGeohashAndDistanceBinPast geohash distanceBin vehicleCategory = "Congestion_geohash_past" <> geohash <> "_distanceBin_" <> distanceBin <> "_vehicleCategory_" <> show vehicleCategory

mkCongestionKeyWithCityPast :: Text -> Maybe DVC.VehicleCategory -> Text
mkCongestionKeyWithCityPast city vehicleCategory = "Congestion_city_past" <> city <> "_vehicleCategory_" <> show vehicleCategory

data DynamicPricingResult = DynamicPricingResult
  { congestionFeePerMin :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    version :: Maybe Text,
    congestionChargeMultiplier :: Maybe FarePolicyD.CongestionChargeMultiplier,
    driverExtraFeeBounds :: Maybe DDriverExtraFeeBounds.DriverExtraFeeBounds
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data DynamicPricingData = DynamicPricingData
  { speedKmh :: Double,
    distanceInKm :: Double,
    actualQAR :: Maybe Double,
    supplyDemandRatioFromLoc :: Double,
    supplyDemandRatioToLoc :: Double,
    actualQARPast :: Maybe Double,
    actualQARToLoc :: Maybe Double,
    actualQARToLocPast :: Maybe Double,
    congestionMultiplier :: Maybe Double,
    congestionMultiplierPast :: Maybe Double,
    serviceTier :: DServiceTierType.ServiceTierType,
    rainStatus :: Maybe Text,
    mbSpecialLocName :: Maybe Text,
    toss :: Int,
    estimatedDurationInS :: Maybe Int,
    actualDurationInS :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default DynamicPricingData where
  def =
    DynamicPricingData
      { speedKmh = 1.1,
        distanceInKm = 1.1,
        supplyDemandRatioFromLoc = 1.1,
        supplyDemandRatioToLoc = 1.1,
        serviceTier = DServiceTierType.TAXI,
        actualQAR = Nothing,
        actualQARPast = Nothing,
        actualQARToLoc = Nothing,
        actualQARToLocPast = Nothing,
        congestionMultiplier = Nothing,
        congestionMultiplierPast = Nothing,
        rainStatus = Nothing,
        mbSpecialLocName = Nothing,
        toss = 1,
        estimatedDurationInS = Nothing,
        actualDurationInS = Nothing
      }
