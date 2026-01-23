{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.MerchantServiceConfig where

import ChatCompletion.Interface.Types
import ChatCompletion.Types
import qualified Data.List as List
import Domain.Types.Common (UsageSafety (..))
import qualified Kernel.External.AadhaarVerification as AadhaarVerification
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.External.BackgroundVerification.Types as BackgroundVerification
import qualified Kernel.External.Call as Call
import Kernel.External.Call.Interface.Types
import qualified Kernel.External.IncidentReport.Interface.Types as IncidentReport
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Interface.Types
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types
import Kernel.External.Payment.Interface as Payment
import Kernel.External.Payout.Interface as Payout
import Kernel.External.SMS as Sms
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Tokenize as Tokenize
import qualified Kernel.External.Verification as Verification
import Kernel.External.Verification.Interface.Types
import Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import Lib.Dashcam.Domain.Interface as DashcamInter
import Lib.Dashcam.Domain.Types as Dashcam
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

-- Extra code goes here --

data ServiceName
  = MapsService Maps.MapsService
  | SmsService Sms.SmsService
  | WhatsappService Whatsapp.WhatsappService
  | VerificationService Verification.VerificationService
  | AadhaarVerificationService AadhaarVerification.AadhaarVerificationService
  | DriverBackgroundVerificationService Verification.DriverBackgroundVerificationService
  | CallService Call.CallService
  | PaymentService Payment.PaymentService
  | RentalPaymentService Payment.PaymentService
  | CautioPaymentService Payment.PaymentService
  | PayoutService Payout.PayoutService
  | RentalPayoutService Payout.PayoutService
  | RidePayoutService Payout.PayoutService
  | IssueTicketService Ticket.IssueTicketService
  | NotificationService Notification.NotificationService
  | TokenizationService Tokenize.TokenizationService
  | BackgroundVerificationService BackgroundVerification.BackgroundVerificationService
  | IncidentReportService IncidentReport.IncidentReportService
  | LLMChatCompletionService ChatCompletion.Types.LLMChatCompletionService
  | DashCamService Dashcam.DashcamService
  | JuspayWalletService Payment.PaymentService
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''ServiceName)

instance Show ServiceName where
  show (MapsService s) = "Maps_" <> show s
  show (SmsService s) = "Sms_" <> show s
  show (WhatsappService s) = "Whatsapp_" <> show s
  show (VerificationService s) = "Verification_" <> show s
  show (AadhaarVerificationService s) = "AadhaarVerification_" <> show s
  show (DriverBackgroundVerificationService s) = "DriverBackgroundVerification_" <> show s
  show (CallService s) = "Call_" <> show s
  show (PaymentService s) = "Payment_" <> show s
  show (RentalPaymentService s) = "RentalPayment_" <> show s
  show (CautioPaymentService s) = "CautioPayment_" <> show s
  show (PayoutService s) = "Payout_" <> show s
  show (RentalPayoutService s) = "RentalPayout_" <> show s
  show (RidePayoutService s) = "RidePayout_" <> show s
  show (IssueTicketService s) = "Ticket_" <> show s
  show (NotificationService s) = "Notification_" <> show s
  show (TokenizationService s) = "Tokenization_" <> show s
  show (BackgroundVerificationService s) = "BackgroundVerification_" <> show s
  show (IncidentReportService s) = "IncidentReport_" <> show s
  show (LLMChatCompletionService s) = "LLMChatCompletion_" <> show s
  show (DashCamService s) = "DashCamService_" <> show s
  show (JuspayWalletService s) = "JuspayWalletService_" <> show s

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
            ++ [ (VerificationService v1, r2)
                 | r1 <- stripPrefix "Verification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (AadhaarVerificationService v1, r2)
                 | r1 <- stripPrefix "AadhaarVerification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (DriverBackgroundVerificationService v1, r2)
                 | r1 <- stripPrefix "DriverBackgroundVerification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (CallService v1, r2)
                 | r1 <- stripPrefix "Call_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (PaymentService v1, r2)
                 | r1 <- stripPrefix "Payment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RentalPaymentService v1, r2)
                 | r1 <- stripPrefix "RentalPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (CautioPaymentService v1, r2)
                 | r1 <- stripPrefix "CautioPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (PayoutService v1, r2)
                 | r1 <- stripPrefix "Payout_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RentalPayoutService v1, r2)
                 | r1 <- stripPrefix "RentalPayout_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RidePayoutService v1, r2)
                 | r1 <- stripPrefix "RidePayout_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (IssueTicketService v1, r2)
                 | r1 <- stripPrefix "Ticket_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (NotificationService v1, r2)
                 | r1 <- stripPrefix "Notification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (TokenizationService v1, r2)
                 | r1 <- stripPrefix "Tokenization_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (BackgroundVerificationService v1, r2)
                 | r1 <- stripPrefix "BackgroundVerification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (IncidentReportService v1, r2)
                 | r1 <- stripPrefix "IncidentReport_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (LLMChatCompletionService v1, r2)
                 | r1 <- stripPrefix "LLMChatCompletion_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (DashCamService v1, r2)
                 | r1 <- stripPrefix "DashCamService_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (JuspayWalletService v1, r2)
                 | r1 <- stripPrefix "JuspayWalletService_" r,
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
  | VerificationServiceConfig !VerificationServiceConfig
  | AadhaarVerificationServiceConfig !AadhaarVerificationServiceConfig
  | DriverBackgroundVerificationServiceConfig !DriverBackgroundVerificationServiceConfig
  | CallServiceConfig !CallServiceConfig
  | PaymentServiceConfig !PaymentServiceConfig
  | RentalPaymentServiceConfig !PaymentServiceConfig
  | CautioPaymentServiceConfig !PaymentServiceConfig
  | PayoutServiceConfig !PayoutServiceConfig
  | RentalPayoutServiceConfig !PayoutServiceConfig
  | RidePayoutServiceConfig !PayoutServiceConfig
  | IssueTicketServiceConfig !Ticket.IssueTicketServiceConfig
  | NotificationServiceConfig !NotificationServiceConfig
  | TokenizationServiceConfig !Tokenize.TokenizationServiceConfig
  | BackgroundVerificationServiceConfig !BackgroundVerification.BackgroundVerificationServiceConfig
  | IncidentReportServiceConfig !IncidentReport.IncidentReportServiceConfig
  | LLMChatCompletionServiceConfig !ChatCompletion.Interface.Types.LLMChatCompletionServiceConfig
  | DashCamServiceConfig !DashcamInter.DashCamServiceConfig
  | JuspayWalletServiceConfig !PaymentServiceConfig
  deriving (Generic, Eq, Show)

type ServiceConfig = ServiceConfigD 'Safe

instance FromJSON (ServiceConfigD 'Unsafe)

instance ToJSON (ServiceConfigD 'Unsafe)

instance FromJSON (ServiceConfigD 'Safe)

instance ToJSON (ServiceConfigD 'Safe)
