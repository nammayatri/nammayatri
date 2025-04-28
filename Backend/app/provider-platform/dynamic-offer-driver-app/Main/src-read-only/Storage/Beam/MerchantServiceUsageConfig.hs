{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantServiceUsageConfig where

import qualified ChatCompletion.Types
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import qualified Kernel.External.AadhaarVerification.Types
import qualified Kernel.External.BackgroundVerification.Types
import qualified Kernel.External.Call
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Notification.Types
import qualified Kernel.External.Payment.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Ticket.Types
import qualified Kernel.External.Verification.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { aadhaarVerificationService :: (B.C f Kernel.External.AadhaarVerification.Types.AadhaarVerificationService),
    autoComplete :: (B.C f Kernel.External.Maps.Types.MapsService),
    backgroundVerification :: (B.C f Kernel.External.BackgroundVerification.Types.BackgroundVerificationService),
    createBankAccount :: (B.C f Kernel.External.Payment.Types.PaymentService),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverBackgroundVerificationService :: (B.C f Kernel.External.Verification.Types.DriverBackgroundVerificationService),
    faceVerificationService :: (B.C f Kernel.External.Verification.Types.VerificationService),
    getBankAccount :: (B.C f Kernel.External.Payment.Types.PaymentService),
    getDistances :: (B.C f Kernel.External.Maps.Types.MapsService),
    getDistancesForCancelRide :: (B.C f Kernel.External.Maps.Types.MapsService),
    getDistancesForScheduledRides :: (B.C f Kernel.External.Maps.Types.MapsService),
    getEstimatedPickupDistances :: (B.C f Kernel.External.Maps.Types.MapsService),
    getExophone :: (B.C f Kernel.External.Call.CallService),
    getPickupRoutes :: (B.C f Kernel.External.Maps.Types.MapsService),
    getPlaceDetails :: (B.C f Kernel.External.Maps.Types.MapsService),
    getPlaceName :: (B.C f Kernel.External.Maps.Types.MapsService),
    getRoutes :: (B.C f Kernel.External.Maps.Types.MapsService),
    getTripRoutes :: (B.C f Kernel.External.Maps.Types.MapsService),
    gstVerificationService :: B.C f (Kernel.Prelude.Maybe Kernel.External.Verification.Types.VerificationService),
    initiateCall :: (B.C f Kernel.External.Call.CallService),
    issueTicketService :: (B.C f Kernel.External.Ticket.Types.IssueTicketService),
    llmChatCompletion :: (B.C f (Kernel.Prelude.Maybe ChatCompletion.Types.LLMChatCompletionService)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    panVerificationService :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Verification.Types.VerificationService)),
    rectifyDistantPointsFailure :: (B.C f Kernel.External.Maps.Types.MapsService),
    retryBankAccountLink :: (B.C f Kernel.External.Payment.Types.PaymentService),
    sdkVerificationService :: (B.C f Kernel.External.Verification.Types.VerificationService),
    sendSearchRequestToDriver :: (B.C f [Kernel.External.Notification.Types.NotificationService]),
    smsProvidersPriorityList :: (B.C f [Kernel.External.SMS.Types.SmsService]),
    snapToRoad :: (B.C f Kernel.External.Maps.Types.MapsService),
    snapToRoadProvidersList :: (B.C f [Kernel.External.Maps.Types.MapsService]),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    verificationProvidersPriorityList :: (B.C f [Kernel.External.Verification.Types.VerificationService]),
    verificationService :: (B.C f Kernel.External.Verification.Types.VerificationService),
    whatsappProvidersPriorityList :: (B.C f [Kernel.External.Whatsapp.Types.WhatsappService])
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceUsageConfigT where
  data PrimaryKey MerchantServiceUsageConfigT f = MerchantServiceUsageConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantServiceUsageConfigId . merchantOperatingCityId

type MerchantServiceUsageConfig = MerchantServiceUsageConfigT Identity

$(enableKVPG (''MerchantServiceUsageConfigT) [('merchantOperatingCityId)] [])

$(mkTableInstances (''MerchantServiceUsageConfigT) "merchant_service_usage_config")

$(Domain.Types.UtilsTH.mkCacParseInstance (''MerchantServiceUsageConfigT))
