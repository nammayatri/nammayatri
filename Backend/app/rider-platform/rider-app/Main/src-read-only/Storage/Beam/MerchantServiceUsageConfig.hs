{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantServiceUsageConfig where

import qualified Database.Beam as B
import qualified Domain.Types.UtilsTH
import qualified Kernel.External.AadhaarVerification
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Notification.Types
import qualified Kernel.External.Payment.Types
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Ticket.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantServiceUsageConfigT f = MerchantServiceUsageConfigT
  { merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    initiateCall :: B.C f Kernel.External.Call.Types.CallService,
    notifyPerson :: B.C f Kernel.External.Notification.Types.NotificationService,
    getDistances :: B.C f Kernel.External.Maps.Types.MapsService,
    getRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    snapToRoad :: B.C f Kernel.External.Maps.Types.MapsService,
    getPlaceName :: B.C f Kernel.External.Maps.Types.MapsService,
    getPickupRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: B.C f Kernel.External.Maps.Types.MapsService,
    autoComplete :: B.C f Kernel.External.Maps.Types.MapsService,
    aadhaarVerificationService :: B.C f Kernel.External.AadhaarVerification.AadhaarVerificationService,
    getDistancesForCancelRide :: B.C f Kernel.External.Maps.Types.MapsService,
    smsProvidersPriorityList :: B.C f [Kernel.External.SMS.Types.SmsService],
    whatsappProvidersPriorityList :: B.C f [Kernel.External.Whatsapp.Types.WhatsappService],
    issueTicketService :: B.C f Kernel.External.Ticket.Types.IssueTicketService,
    useFraudDetection :: B.C f Kernel.Prelude.Bool,
    enableDashboardSms :: B.C f Kernel.Prelude.Bool,
    getExophone :: B.C f Kernel.External.Call.Types.CallService,
    createPaymentCustomer :: B.C f Kernel.External.Payment.Types.PaymentService,
    createEphemeralKeys :: B.C f Kernel.External.Payment.Types.PaymentService,
    getCardList :: B.C f Kernel.External.Payment.Types.PaymentService,
    createPaymentIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    updatePaymentMethodInIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    capturePaymentIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    updateAmountInPaymentIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    createSetupIntent :: B.C f Kernel.External.Payment.Types.PaymentService,
    deleteCard :: B.C f Kernel.External.Payment.Types.PaymentService,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    createdAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantServiceUsageConfigT where
  data PrimaryKey MerchantServiceUsageConfigT f = MerchantServiceUsageConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantServiceUsageConfigId . merchantOperatingCityId

type MerchantServiceUsageConfig = MerchantServiceUsageConfigT Identity

$(enableKVPG ''MerchantServiceUsageConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''MerchantServiceUsageConfigT "merchant_service_usage_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''MerchantServiceUsageConfigT)
