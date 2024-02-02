{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Merchant.MerchantServiceUsageConfig where

import qualified Data.Aeson as A
import qualified Database.Beam as B
import Kernel.External.AadhaarVerification.Types
import Kernel.External.Call (CallService)
import Kernel.External.Maps.Types
import Kernel.External.Notification.Types (NotificationService)
import Kernel.External.SMS.Types
import Kernel.External.Ticket.Types
import Kernel.External.Verification.Types
import Kernel.External.Whatsapp.Types
import Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    initiateCall :: B.C f CallService,
    getDistances :: B.C f MapsService,
    getDistancesPercentage :: B.C f A.Value,
    getEstimatedPickupDistances :: B.C f MapsService,
    getEstimatedPickupDistancesPercentage :: B.C f A.Value,
    getRoutes :: B.C f MapsService,
    getRoutesPercentage :: B.C f A.Value,
    getPickupRoutes :: B.C f MapsService,
    getPickupRoutesPercentage :: B.C f A.Value,
    getTripRoutes :: B.C f MapsService,
    getTripRoutesPercentage :: B.C f A.Value,
    snapToRoad :: B.C f MapsService,
    snapToRoadPercentage :: B.C f A.Value,
    getPlaceName :: B.C f MapsService,
    getPlaceNamePercentage :: B.C f A.Value,
    getPlaceDetails :: B.C f MapsService,
    getPlaceDetailsPercentage :: B.C f A.Value,
    autoComplete :: B.C f MapsService,
    autoCompletePercentage :: B.C f A.Value,
    getDistancesForCancelRide :: B.C f MapsService,
    getDistancesForCancelRidePercentage :: B.C f A.Value,
    smsProvidersPriorityList :: B.C f [SmsService],
    snapToRoadProvidersList :: B.C f [MapsService],
    whatsappProvidersPriorityList :: B.C f [WhatsappService],
    verificationService :: B.C f VerificationService,
    faceVerificationService :: B.C f VerificationService,
    sendSearchRequestToDriver :: B.C f [NotificationService],
    aadhaarVerificationService :: B.C f AadhaarVerificationService,
    issueTicketService :: B.C f IssueTicketService,
    getExophone :: B.C f CallService,
    updatedAt :: B.C f UTCTime,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceUsageConfigT where
  data PrimaryKey MerchantServiceUsageConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantOperatingCityId

type MerchantServiceUsageConfig = MerchantServiceUsageConfigT Identity

$(enableKVPG ''MerchantServiceUsageConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''MerchantServiceUsageConfigT "merchant_service_usage_config")
