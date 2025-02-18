module Domain.Types.Extra.MerchantServiceConfig where

import qualified Data.List as List
import Domain.Types.Common (UsageSafety (..))
import qualified Kernel.External.AadhaarVerification as AadhaarVerification
import Kernel.External.AadhaarVerification.Interface.Types
import qualified Kernel.External.Call as Call
import Kernel.External.Call.Interface.Types
import qualified Kernel.External.IncidentReport.Interface.Types as IncidentReport
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
  | IssueTicketService Ticket.IssueTicketService
  | TokenizationService Tokenize.TokenizationService
  | IncidentReportService IncidentReport.IncidentReportService
  | PayoutService Payout.PayoutService
  | MultiModalService MultiModal.MultiModalService
  | WalletService GW.WalletService
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
  show (IssueTicketService s) = "Ticket_" <> show s
  show (TokenizationService s) = "Tokenization_" <> show s
  show (IncidentReportService s) = "IncidentReport_" <> show s
  show (PayoutService s) = "Payout_" <> show s
  show (MultiModalService s) = "MultiModal_" <> show s
  show (WalletService s) = "Wallet_" <> show s

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
  | IssueTicketServiceConfig !Ticket.IssueTicketServiceConfig
  | TokenizationServiceConfig !Tokenize.TokenizationServiceConfig
  | IncidentReportServiceConfig !IncidentReport.IncidentReportServiceConfig
  | PayoutServiceConfig !PayoutServiceConfig
  | MultiModalServiceConfig !MultiModal.MultiModalServiceConfig
  | WalletServiceConfig !GW.WalletServiceConfig
  deriving (Generic, Eq)

type ServiceConfig = ServiceConfigD 'Safe

instance FromJSON (ServiceConfigD 'Unsafe)

instance ToJSON (ServiceConfigD 'Unsafe)

instance FromJSON (ServiceConfigD 'Safe)

instance ToJSON (ServiceConfigD 'Safe)
