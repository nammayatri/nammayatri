module Domain.Types.Extra.MerchantServiceConfig where

import qualified Data.List as List
import Domain.Types.Common (UsageSafety (..))
import qualified Kernel.External.AadhaarVerification as AadhaarVerification
import Kernel.External.AadhaarVerification.Interface.Types
import qualified Kernel.External.Call as Call
import Kernel.External.Call.Interface.Types
import qualified Kernel.External.IncidentReport.Interface.Types as IncidentReport
import qualified Kernel.External.Insurance.Interface.Types as Insurance
import qualified Kernel.External.Insurance.Types as Insurance
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Interface.Types
import Kernel.External.MultiModal.Interface.Types as MultiModal
import Kernel.External.MultiModal.Types as MultiModal
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types
import Kernel.External.Payment.Interface as Payment
import Kernel.External.Payout.Interface as Payout
import Kernel.External.SMS as Sms
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Tokenize as Tokenize
import Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import qualified Text.Show as Show
import Tools.Beam.UtilsTH
import Utils.Common.JWT.Config as GW

-- Extra code goes here --
data ServiceName
  = MapsService Maps.MapsService
  | SmsService Sms.SmsService
  | WhatsappService Whatsapp.WhatsappService
  | AadhaarVerificationService AadhaarVerification.AadhaarVerificationService
  | CallService Call.CallService
  | NotificationService Notification.NotificationService
  | PaymentService Payment.PaymentService
  | MetroPaymentService Payment.PaymentService
  | BusPaymentService Payment.PaymentService
  | BbpsPaymentService Payment.PaymentService
  | MultiModalPaymentService Payment.PaymentService
  | PassPaymentService Payment.PaymentService
  | ParkingPaymentService Payment.PaymentService
  | IssueTicketService Ticket.IssueTicketService
  | TokenizationService Tokenize.TokenizationService
  | IncidentReportService IncidentReport.IncidentReportService
  | PayoutService Payout.PayoutService
  | MultiModalService MultiModal.MultiModalService
  | WalletService GW.WalletService
  | MultiModalStaticDataService MultiModal.MultiModalService
  | InsuranceService Insurance.InsuranceService
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''ServiceName)

instance Show ServiceName where
  show (MapsService s) = "Maps_" <> show s
  show (SmsService s) = "Sms_" <> show s
  show (WhatsappService s) = "Whatsapp_" <> show s
  show (AadhaarVerificationService s) = "AadhaarVerification_" <> show s
  show (CallService s) = "Call_" <> show s
  show (NotificationService s) = "Notification_" <> show s
  show (PaymentService s) = "Payment_" <> show s
  show (MetroPaymentService s) = "MetroPayment_" <> show s
  show (BusPaymentService s) = "BusPayment_" <> show s
  show (BbpsPaymentService s) = "BbpsPayment_" <> show s
  show (MultiModalPaymentService s) = "MultiModalPayment_" <> show s
  show (PassPaymentService s) = "PassPayment_" <> show s
  show (ParkingPaymentService s) = "ParkingPayment_" <> show s
  show (IssueTicketService s) = "Ticket_" <> show s
  show (TokenizationService s) = "Tokenization_" <> show s
  show (IncidentReportService s) = "IncidentReport_" <> show s
  show (PayoutService s) = "Payout_" <> show s
  show (MultiModalService s) = "MultiModal_" <> show s
  show (WalletService s) = "Wallet_" <> show s
  show (MultiModalStaticDataService s) = "MultiModalStaticData_" <> show s
  show (InsuranceService s) = "Insurance_" <> show s

instance Read ServiceName where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (MapsService v1, r2)
            | r1 <- stripPrefix "Maps_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (SmsService v1, r2)
                 | r1 <- stripPrefix "Sms_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (WhatsappService v1, r2)
                 | r1 <- stripPrefix "Whatsapp_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (AadhaarVerificationService v1, r2)
                 | r1 <- stripPrefix "AadhaarVerification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (CallService v1, r2)
                 | r1 <- stripPrefix "Call_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (NotificationService v1, r2)
                 | r1 <- stripPrefix "Notification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (PaymentService v1, r2)
                 | r1 <- stripPrefix "Payment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MetroPaymentService v1, r2)
                 | r1 <- stripPrefix "MetroPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (BusPaymentService v1, r2)
                 | r1 <- stripPrefix "BusPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (BbpsPaymentService v1, r2)
                 | r1 <- stripPrefix "BbpsPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MultiModalPaymentService v1, r2)
                 | r1 <- stripPrefix "MultiModalPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (PassPaymentService v1, r2)
                 | r1 <- stripPrefix "PassPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ParkingPaymentService v1, r2)
                 | r1 <- stripPrefix "ParkingPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (IssueTicketService v1, r2)
                 | r1 <- stripPrefix "Ticket_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (TokenizationService v1, r2)
                 | r1 <- stripPrefix "Tokenization_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (IncidentReportService v1, r2)
                 | r1 <- stripPrefix "IncidentReport_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (PayoutService v1, r2)
                 | r1 <- stripPrefix "Payout_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MultiModalService v1, r2)
                 | r1 <- stripPrefix "MultiModal_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (WalletService v1, r2)
                 | r1 <- stripPrefix "Wallet_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MultiModalStaticDataService v1, r2)
                 | r1 <- stripPrefix "MultiModalStaticData_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (InsuranceService v1, r2)
                 | r1 <- stripPrefix "Insurance_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

data ServiceConfigD (s :: UsageSafety)
  = MapsServiceConfig !MapsServiceConfig
  | SmsServiceConfig !SmsServiceConfig
  | WhatsappServiceConfig !WhatsappServiceConfig
  | AadhaarVerificationServiceConfig !AadhaarVerificationServiceConfig
  | CallServiceConfig !CallServiceConfig
  | NotificationServiceConfig !NotificationServiceConfig
  | PaymentServiceConfig !PaymentServiceConfig
  | MetroPaymentServiceConfig !PaymentServiceConfig
  | BusPaymentServiceConfig !PaymentServiceConfig
  | BbpsPaymentServiceConfig !PaymentServiceConfig
  | MultiModalPaymentServiceConfig !PaymentServiceConfig
  | PassPaymentServiceConfig !PaymentServiceConfig
  | ParkingPaymentServiceConfig !PaymentServiceConfig
  | IssueTicketServiceConfig !Ticket.IssueTicketServiceConfig
  | TokenizationServiceConfig !Tokenize.TokenizationServiceConfig
  | IncidentReportServiceConfig !IncidentReport.IncidentReportServiceConfig
  | PayoutServiceConfig !PayoutServiceConfig
  | MultiModalServiceConfig !MultiModal.MultiModalServiceConfig
  | WalletServiceConfig !GW.WalletServiceConfig
  | MultiModalStaticDataServiceConfig !MultiModal.MultiModalServiceConfig
  | InsuranceServiceConfig !Insurance.InsuranceConfig
  deriving (Generic, Eq)

type ServiceConfig = ServiceConfigD 'Safe

instance FromJSON (ServiceConfigD 'Unsafe)

instance ToJSON (ServiceConfigD 'Unsafe)

instance FromJSON (ServiceConfigD 'Safe)

instance ToJSON (ServiceConfigD 'Safe)

instance Show (ServiceConfigD 'Safe) where
  show (MapsServiceConfig cfg) = "MapsServiceConfig " <> show cfg
  show (SmsServiceConfig cfg) = "SmsServiceConfig " <> show cfg
  show (WhatsappServiceConfig cfg) = "WhatsappServiceConfig " <> show cfg
  show (AadhaarVerificationServiceConfig cfg) = "AadhaarVerificationServiceConfig " <> show cfg
  show (CallServiceConfig cfg) = "CallServiceConfig " <> show cfg
  show (NotificationServiceConfig cfg) = "NotificationServiceConfig " <> show cfg
  show (PaymentServiceConfig cfg) = "PaymentServiceConfig " <> show cfg
  show (MetroPaymentServiceConfig cfg) = "MetroPaymentServiceConfig " <> show cfg
  show (BusPaymentServiceConfig cfg) = "BusPaymentServiceConfig " <> show cfg
  show (BbpsPaymentServiceConfig cfg) = "BbpsPaymentServiceConfig " <> show cfg
  show (MultiModalPaymentServiceConfig cfg) = "MultiModalPaymentServiceConfig " <> show cfg
  show (PassPaymentServiceConfig cfg) = "PassPaymentServiceConfig " <> show cfg
  show (ParkingPaymentServiceConfig cfg) = "ParkingPaymentServiceConfig " <> show cfg
  show (IssueTicketServiceConfig cfg) = "IssueTicketServiceConfig " <> show cfg
  show (TokenizationServiceConfig cfg) = "TokenizationServiceConfig " <> show cfg
  show (IncidentReportServiceConfig cfg) = "IncidentReportServiceConfig " <> show cfg
  show (PayoutServiceConfig cfg) = "PayoutServiceConfig " <> show cfg
  show (MultiModalServiceConfig cfg) = "MultiModalServiceConfig " <> show cfg
  show (WalletServiceConfig cfg) = "WalletServiceConfig " <> show cfg
  show (MultiModalStaticDataServiceConfig cfg) = "MultiModalStaticDataServiceConfig " <> show cfg
  show (InsuranceServiceConfig cfg) = "InsuranceServiceConfig " <> show cfg

instance Show (ServiceConfigD 'Unsafe) where
  show (MapsServiceConfig cfg) = "MapsServiceConfig " <> show cfg
  show (SmsServiceConfig cfg) = "SmsServiceConfig " <> show cfg
  show (WhatsappServiceConfig cfg) = "WhatsappServiceConfig " <> show cfg
  show (AadhaarVerificationServiceConfig cfg) = "AadhaarVerificationServiceConfig " <> show cfg
  show (CallServiceConfig cfg) = "CallServiceConfig " <> show cfg
  show (NotificationServiceConfig cfg) = "NotificationServiceConfig " <> show cfg
  show (PaymentServiceConfig cfg) = "PaymentServiceConfig " <> show cfg
  show (MetroPaymentServiceConfig cfg) = "MetroPaymentServiceConfig " <> show cfg
  show (BusPaymentServiceConfig cfg) = "BusPaymentServiceConfig " <> show cfg
  show (BbpsPaymentServiceConfig cfg) = "BbpsPaymentServiceConfig " <> show cfg
  show (MultiModalPaymentServiceConfig cfg) = "MultiModalPaymentServiceConfig " <> show cfg
  show (PassPaymentServiceConfig cfg) = "PassPaymentServiceConfig " <> show cfg
  show (ParkingPaymentServiceConfig cfg) = "ParkingPaymentServiceConfig " <> show cfg
  show (IssueTicketServiceConfig cfg) = "IssueTicketServiceConfig " <> show cfg
  show (TokenizationServiceConfig cfg) = "TokenizationServiceConfig " <> show cfg
  show (IncidentReportServiceConfig cfg) = "IncidentReportServiceConfig " <> show cfg
  show (PayoutServiceConfig cfg) = "PayoutServiceConfig " <> show cfg
  show (MultiModalServiceConfig cfg) = "MultiModalServiceConfig " <> show cfg
  show (WalletServiceConfig cfg) = "WalletServiceConfig " <> show cfg
  show (MultiModalStaticDataServiceConfig cfg) = "MultiModalStaticDataServiceConfig " <> show cfg
  show (InsuranceServiceConfig cfg) = "InsuranceServiceConfig " <> show cfg
