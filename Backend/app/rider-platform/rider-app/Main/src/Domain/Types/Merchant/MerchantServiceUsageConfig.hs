{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.MerchantServiceUsageConfig where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.External.AadhaarVerification
import Kernel.External.Call.Types (CallService)
import Kernel.External.Maps.Types
import Kernel.External.Notification.Types
import Kernel.External.SMS.Types
import Kernel.External.Ticket.Types (IssueTicketService)
import Kernel.External.Whatsapp.Types
import Kernel.Prelude
import Kernel.Types.Id

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    initiateCall :: CallService,
    notifyPerson :: NotificationService,
    getDistances :: MapsService,
    getRoutes :: MapsService,
    snapToRoad :: MapsService,
    getPlaceName :: MapsService,
    getPickupRoutes :: MapsService,
    getTripRoutes :: MapsService,
    getPlaceDetails :: MapsService,
    autoComplete :: MapsService,
    aadhaarVerificationService :: AadhaarVerificationService,
    getDistancesForCancelRide :: MapsService,
    smsProvidersPriorityList :: [SmsService],
    whatsappProvidersPriorityList :: [WhatsappService],
    issueTicketService :: IssueTicketService,
    useFraudDetection :: Bool,
    enableDashboardSms :: Bool,
    getExophone :: CallService,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)
