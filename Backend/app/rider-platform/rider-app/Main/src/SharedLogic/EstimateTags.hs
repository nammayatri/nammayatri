module SharedLogic.EstimateTags where

import BecknV2.OnDemand.Enums
import Data.Default.Class
import qualified Domain.Types.ServiceTierType as DST
import Kernel.Prelude
import Kernel.Types.Common

data EstimateTagsData = EstimateTagsData
  { autoQAR :: Maybe Double,
    cabQAR :: Maybe Double,
    autoPrice :: Maybe HighPrecMoney,
    cabPrice :: Maybe HighPrecMoney,
    userType :: Maybe UserType,
    vehicleCategory :: Maybe VehicleCategory,
    vehicleServiceTierType :: DST.ServiceTierType
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data UserType = AUTO | CAB
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default EstimateTagsData where
  def =
    EstimateTagsData
      { autoQAR = Nothing,
        cabQAR = Nothing,
        autoPrice = Nothing,
        cabPrice = Nothing,
        userType = Nothing,
        vehicleCategory = Nothing,
        vehicleServiceTierType = DST.AUTO_RICKSHAW
      }

newtype EstimateTagsResult = EstimateTagsResult
  { tags :: [Text]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default EstimateTagsResult where
  def = EstimateTagsResult {tags = []}
