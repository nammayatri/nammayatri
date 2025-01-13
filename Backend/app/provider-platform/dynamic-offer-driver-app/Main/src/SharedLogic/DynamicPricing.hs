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
import qualified Domain.Types.ServiceTierType as DServiceTierType
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common

mkSupplyDemandRatioKeyWithGeohash :: Text -> DServiceTierType.ServiceTierType -> Text
mkSupplyDemandRatioKeyWithGeohash geohash vehicleServiceTier = "S_D_ratio_geohash" <> geohash <> "_serviceTier_" <> show vehicleServiceTier

data DynamicPricingResult = DynamicPricingResult
  { congestionFeePerMin :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    version :: Maybe Text,
    congestionChargeMultiplier :: Maybe FarePolicyD.CongestionChargeMultiplier
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data DynamicPricingData = DynamicPricingData
  { speedKmh :: Double,
    distanceInKm :: Double,
    supplyDemandRatioFromLoc :: Double,
    supplyDemandRatioToLoc :: Double,
    serviceTier :: DServiceTierType.ServiceTierType,
    toss :: Int --,
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-----------------Default values to be changed------------RITIKA
instance Default DynamicPricingData where
  def =
    DynamicPricingData
      { speedKmh = 1.1,
        distanceInKm = 1.1,
        supplyDemandRatioFromLoc = 1.1,
        supplyDemandRatioToLoc = 1.1,
        serviceTier = DServiceTierType.TAXI,
        toss = 1 --,
      }
