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
import qualified Kernel.External.Payment.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Ticket.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    initiateCall :: Kernel.External.Call.Types.CallService,
    notifyPerson :: Kernel.External.Notification.Types.NotificationService,
    getDistances :: Kernel.External.Maps.Types.MapsService,
    getRoutes :: Kernel.External.Maps.Types.MapsService,
    snapToRoad :: Kernel.External.Maps.Types.MapsService,
    getPlaceName :: Kernel.External.Maps.Types.MapsService,
    getPickupRoutes :: Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: Kernel.External.Maps.Types.MapsService,
    autoComplete :: Kernel.External.Maps.Types.MapsService,
    aadhaarVerificationService :: Kernel.External.AadhaarVerification.AadhaarVerificationService,
    getDistancesForCancelRide :: Kernel.External.Maps.Types.MapsService,
    smsProvidersPriorityList :: [Kernel.External.SMS.Types.SmsService],
    whatsappProvidersPriorityList :: [Kernel.External.Whatsapp.Types.WhatsappService],
    issueTicketService :: Kernel.External.Ticket.Types.IssueTicketService,
    useFraudDetection :: Kernel.Prelude.Bool,
    enableDashboardSms :: Kernel.Prelude.Bool,
    getExophone :: Kernel.External.Call.Types.CallService,
    createPaymentCustomer :: Kernel.External.Payment.Types.PaymentService,
    createEphemeralKeys :: Kernel.External.Payment.Types.PaymentService,
    getCardList :: Kernel.External.Payment.Types.PaymentService,
    createPaymentIntent :: Kernel.External.Payment.Types.PaymentService,
    updatePaymentMethodInIntent :: Kernel.External.Payment.Types.PaymentService,
    capturePaymentIntent :: Kernel.External.Payment.Types.PaymentService,
    updateAmountInPaymentIntent :: Kernel.External.Payment.Types.PaymentService,
    createSetupIntent :: Kernel.External.Payment.Types.PaymentService,
    deleteCard :: Kernel.External.Payment.Types.PaymentService,
    updatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)

instance FromJSON (MerchantServiceUsageConfigD 'Safe)

instance ToJSON (MerchantServiceUsageConfigD 'Safe)
