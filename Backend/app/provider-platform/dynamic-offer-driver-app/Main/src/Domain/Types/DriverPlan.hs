{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DriverPlan where

import qualified Data.Aeson as A
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Mandate as DM
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data DriverPlan = DriverPlan
  { driverId :: Id DP.Person,
    planId :: Id DPlan.Plan,
    planType :: DPlan.PaymentMode,
    mandateId :: Maybe (Id DM.Mandate),
    mandateSetupDate :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    coinCovertedToCashLeft :: HighPrecMoney,
    totalCoinsConvertedCash :: HighPrecMoney,
    payerVpa :: Maybe Text,
    autoPayStatus :: Maybe DI.DriverAutoPayStatus,
    serviceName :: DPlan.ServiceNames,
    enableServiceUsageCharge :: Bool,
    merchantId :: Id Merchant,
    merchantOpCityId :: Id MerchantOperatingCity,
    subscriptionServiceRelatedData :: SubscriptionServiceRelatedData
  }
  deriving (Generic, Show)

newtype CommodityData = CommodityData
  { rentedVehicleNumber :: Maybe Text
  }
  deriving (Generic, Show, Ord, Eq)

data SubscriptionServiceRelatedData = RentedVehicleNumber Text | NoData
  deriving (Generic, Show, Ord, Eq)

instance ToJSON CommodityData where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON CommodityData where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SubscriptionServiceRelatedData where
  toJSON (RentedVehicleNumber vehicleNumber) = A.object ["rentedVehicleNumber" A..= vehicleNumber]
  toJSON NoData = A.object []

instance FromJSON SubscriptionServiceRelatedData where
  parseJSON = A.withObject "SubscriptionServiceRelatedData" $ \o -> do
    rentedVehicleNumber <- o A..:? "rentedVehicleNumber"
    case rentedVehicleNumber of
      Just vehicleNumber -> return (RentedVehicleNumber vehicleNumber)
      Nothing -> return NoData
