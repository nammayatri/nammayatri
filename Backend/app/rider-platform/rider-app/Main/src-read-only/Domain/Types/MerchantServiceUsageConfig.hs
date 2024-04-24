{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantServiceUsageConfig where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.AadhaarVerification
import qualified Kernel.External.Call.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Notification.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Ticket.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { aadhaarVerificationService :: Kernel.External.AadhaarVerification.AadhaarVerificationService,
    autoComplete :: Kernel.External.Maps.Types.MapsService,
    createdAt :: Kernel.Prelude.UTCTime,
    enableDashboardSms :: Kernel.Prelude.Bool,
    getDistances :: Kernel.External.Maps.Types.MapsService,
    getDistancesForCancelRide :: Kernel.External.Maps.Types.MapsService,
    getExophone :: Kernel.External.Call.Types.CallService,
    getPickupRoutes :: Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: Kernel.External.Maps.Types.MapsService,
    getPlaceName :: Kernel.External.Maps.Types.MapsService,
    getRoutes :: Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: Kernel.External.Maps.Types.MapsService,
    initiateCall :: Kernel.External.Call.Types.CallService,
    issueTicketService :: Kernel.External.Ticket.Types.IssueTicketService,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notifyPerson :: Kernel.External.Notification.Types.NotificationService,
    smsProvidersPriorityList :: [Kernel.External.SMS.Types.SmsService],
    snapToRoad :: Kernel.External.Maps.Types.MapsService,
    updatedAt :: Kernel.Prelude.UTCTime,
    useFraudDetection :: Kernel.Prelude.Bool,
    whatsappProvidersPriorityList :: [Kernel.External.Whatsapp.Types.WhatsappService]
  }
  deriving (Generic, Show)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)
