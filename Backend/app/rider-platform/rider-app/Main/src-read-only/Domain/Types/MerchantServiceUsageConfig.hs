{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantServiceUsageConfig where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.AadhaarVerification
import qualified Kernel.External.Call.Types
import qualified Kernel.External.Insurance.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Types
import qualified Kernel.External.Notification.Types
import qualified Kernel.External.Payment.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Ticket.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { aadhaarVerificationService :: Kernel.External.AadhaarVerification.AadhaarVerificationService,
    autoComplete :: Kernel.External.Maps.Types.MapsService,
    cancelPaymentIntent :: Kernel.External.Payment.Types.PaymentService,
    capturePaymentIntent :: Kernel.External.Payment.Types.PaymentService,
    createEphemeralKeys :: Kernel.External.Payment.Types.PaymentService,
    createPaymentCustomer :: Kernel.External.Payment.Types.PaymentService,
    createPaymentIntent :: Kernel.External.Payment.Types.PaymentService,
    createRefunds :: Kernel.External.Payment.Types.PaymentService,
    createSetupIntent :: Kernel.External.Payment.Types.PaymentService,
    createdAt :: Kernel.Prelude.UTCTime,
    deleteCard :: Kernel.External.Payment.Types.PaymentService,
    enableDashboardSms :: Kernel.Prelude.Bool,
    getCardList :: Kernel.External.Payment.Types.PaymentService,
    getDistances :: Kernel.External.Maps.Types.MapsService,
    getDistancesForCancelRide :: Kernel.External.Maps.Types.MapsService,
    getDistancesForScheduledRides :: Kernel.External.Maps.Types.MapsService,
    getExophone :: Kernel.External.Call.Types.CallService,
    getFirstPickupRoute :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    getFrfsAutocompleteDistances :: Kernel.External.Maps.Types.MapsService,
    getMultiModalService :: Kernel.External.MultiModal.Types.MultiModalService,
    getMultimodalWalkDistance :: Kernel.External.Maps.Types.MapsService,
    getPickupRoutes :: Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: Kernel.External.Maps.Types.MapsService,
    getPlaceName :: Kernel.External.Maps.Types.MapsService,
    getRefunds :: Kernel.External.Payment.Types.PaymentService,
    getRoutes :: Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: Kernel.External.Maps.Types.MapsService,
    initiateCall :: Kernel.External.Call.Types.CallService,
    insuranceService :: Kernel.External.Insurance.Types.InsuranceService,
    issueTicketService :: Kernel.External.Ticket.Types.IssueTicketService,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notifyPerson :: Kernel.External.Notification.Types.NotificationService,
    smsProvidersPriorityList :: [Kernel.External.SMS.Types.SmsService],
    snapToRoad :: Kernel.External.Maps.Types.MapsService,
    updateAmountInPaymentIntent :: Kernel.External.Payment.Types.PaymentService,
    updatePaymentMethodInIntent :: Kernel.External.Payment.Types.PaymentService,
    updatedAt :: Kernel.Prelude.UTCTime,
    useFraudDetection :: Kernel.Prelude.Bool,
    whatsappProvidersPriorityList :: [Kernel.External.Whatsapp.Types.WhatsappService]
  }
  deriving (Generic, Show)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)

instance FromJSON (MerchantServiceUsageConfigD 'Safe)

instance ToJSON (MerchantServiceUsageConfigD 'Safe)
