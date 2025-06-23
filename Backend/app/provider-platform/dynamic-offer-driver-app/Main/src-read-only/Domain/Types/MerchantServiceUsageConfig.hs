{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantServiceUsageConfig where

import qualified ChatCompletion.Types
import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.AadhaarVerification.Types
import qualified Kernel.External.BackgroundVerification.Types
import qualified Kernel.External.Call
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Notification.Types
import qualified Kernel.External.Payment.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Ticket.Types
import qualified Kernel.External.Verification.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { aadhaarVerificationService :: Kernel.External.AadhaarVerification.Types.AadhaarVerificationService,
    autoComplete :: Kernel.External.Maps.Types.MapsService,
    backgroundVerification :: Kernel.External.BackgroundVerification.Types.BackgroundVerificationService,
    createBankAccount :: Kernel.External.Payment.Types.PaymentService,
    createdAt :: Kernel.Prelude.UTCTime,
    driverBackgroundVerificationService :: Kernel.External.Verification.Types.DriverBackgroundVerificationService,
    faceVerificationService :: Kernel.External.Verification.Types.VerificationService,
    getBankAccount :: Kernel.External.Payment.Types.PaymentService,
    getDistances :: Kernel.External.Maps.Types.MapsService,
    getDistancesForCancelRide :: Kernel.External.Maps.Types.MapsService,
    getDistancesForScheduledRides :: Kernel.External.Maps.Types.MapsService,
    getEstimatedPickupDistances :: Kernel.External.Maps.Types.MapsService,
    getExophone :: Kernel.External.Call.CallService,
    getPickupRoutes :: Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: Kernel.External.Maps.Types.MapsService,
    getPlaceName :: Kernel.External.Maps.Types.MapsService,
    getRoutes :: Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: Kernel.External.Maps.Types.MapsService,
    gstVerificationService :: Kernel.Prelude.Maybe Kernel.External.Verification.Types.VerificationService,
    initiateCall :: Kernel.External.Call.CallService,
    issueTicketService :: Kernel.External.Ticket.Types.IssueTicketService,
    llmChatCompletion :: ChatCompletion.Types.LLMChatCompletionService,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    panVerificationService :: Kernel.Prelude.Maybe Kernel.External.Verification.Types.VerificationService,
    rectifyDistantPointsFailure :: Kernel.External.Maps.Types.MapsService,
    retryBankAccountLink :: Kernel.External.Payment.Types.PaymentService,
    sdkVerificationService :: Kernel.External.Verification.Types.VerificationService,
    sendSearchRequestToDriver :: [Kernel.External.Notification.Types.NotificationService],
    smsProvidersPriorityList :: [Kernel.External.SMS.Types.SmsService],
    snapToRoad :: Kernel.External.Maps.Types.MapsService,
    snapToRoadProvidersList :: [Kernel.External.Maps.Types.MapsService],
    updatedAt :: Kernel.Prelude.UTCTime,
    verificationProvidersPriorityList :: [Kernel.External.Verification.Types.VerificationService],
    verificationService :: Kernel.External.Verification.Types.VerificationService,
    whatsappProvidersPriorityList :: [Kernel.External.Whatsapp.Types.WhatsappService]
  }
  deriving (Generic, Show)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD ('Safe)

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)

instance FromJSON (MerchantServiceUsageConfigD 'Safe)

instance ToJSON (MerchantServiceUsageConfigD 'Safe)
