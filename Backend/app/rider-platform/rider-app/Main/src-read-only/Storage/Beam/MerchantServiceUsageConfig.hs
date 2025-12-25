{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantServiceUsageConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import qualified Kernel.External.AadhaarVerification
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import qualified Kernel.External.Insurance.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Types
import qualified Kernel.External.Notification.Types
import qualified Kernel.External.Payment.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Ticket.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { aadhaarVerificationService :: B.C f Kernel.External.AadhaarVerification.AadhaarVerificationService,
    autoComplete :: B.C f Kernel.External.Maps.Types.MapsService,
    cancelPaymentIntent :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payment.Types.PaymentService),
    capturePaymentIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    createEphemeralKeys :: B.C f Kernel.External.Payment.Types.PaymentService,
    createPaymentCustomer :: B.C f Kernel.External.Payment.Types.PaymentService,
    createPaymentIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    createSetupIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    deleteCard :: B.C f Kernel.External.Payment.Types.PaymentService,
    enableDashboardSms :: B.C f Kernel.Prelude.Bool,
    getCardList :: B.C f Kernel.External.Payment.Types.PaymentService,
    getDistances :: B.C f Kernel.External.Maps.Types.MapsService,
    getDistancesForCancelRide :: B.C f Kernel.External.Maps.Types.MapsService,
    getDistancesForScheduledRides :: B.C f Kernel.External.Maps.Types.MapsService,
    getExophone :: B.C f Kernel.External.Call.Types.CallService,
    getFirstPickupRoute :: B.C f (Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService),
    getFrfsAutocompleteDistances :: B.C f (Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService),
    getMultiModalService :: B.C f (Kernel.Prelude.Maybe Kernel.External.MultiModal.Types.MultiModalService),
    getMultimodalWalkDistance :: B.C f (Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService),
    getPickupRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: B.C f Kernel.External.Maps.Types.MapsService,
    getPlaceName :: B.C f Kernel.External.Maps.Types.MapsService,
    getRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    initiateCall :: B.C f Kernel.External.Call.Types.CallService,
    insuranceService :: B.C f (Kernel.Prelude.Maybe Kernel.External.Insurance.Types.InsuranceService),
    issueTicketService :: B.C f Kernel.External.Ticket.Types.IssueTicketService,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    notifyPerson :: B.C f Kernel.External.Notification.Types.NotificationService,
    smsProvidersPriorityList :: B.C f [Kernel.External.SMS.Types.SmsService],
    snapToRoad :: B.C f Kernel.External.Maps.Types.MapsService,
    updateAmountInPaymentIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    updatePaymentMethodInIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    useFraudDetection :: B.C f Kernel.Prelude.Bool,
    whatsappProvidersPriorityList :: B.C f [Kernel.External.Whatsapp.Types.WhatsappService]
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceUsageConfigT where
  data PrimaryKey MerchantServiceUsageConfigT f = MerchantServiceUsageConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantServiceUsageConfigId . merchantOperatingCityId

type MerchantServiceUsageConfig = MerchantServiceUsageConfigT Identity

$(enableKVPG ''MerchantServiceUsageConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''MerchantServiceUsageConfigT "merchant_service_usage_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''MerchantServiceUsageConfigT)
