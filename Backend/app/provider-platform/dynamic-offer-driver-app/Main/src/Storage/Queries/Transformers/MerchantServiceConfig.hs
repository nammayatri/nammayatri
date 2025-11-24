module Storage.Queries.Transformers.MerchantServiceConfig where

import ChatCompletion.Interface.Types as CIT
import ChatCompletion.Types
import qualified Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.MerchantServiceConfig as Domain
import qualified Kernel.External.AadhaarVerification.Interface as AadhaarVerification
import qualified Kernel.External.BackgroundVerification.Types as BackgroundVerification
import qualified Kernel.External.Call as Call
import Kernel.External.IncidentReport.Interface.Types as IncidentReport
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types as Notification
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.External.SMS.Interface as Sms
import Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Tokenize as Tokenize
import qualified Kernel.External.Verification.Interface as Verification
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude as P
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.Dashcam.Domain.Interface as DashcamInter
import qualified Lib.Dashcam.Domain.Types as Dashcam

getConfigJSON :: Domain.ServiceConfig -> Data.Aeson.Value
getConfigJSON = \case
  Domain.MapsServiceConfig mapsCfg -> case mapsCfg of
    Maps.GoogleConfig cfg -> toJSON cfg
    Maps.OSRMConfig cfg -> toJSON cfg
    Maps.MMIConfig cfg -> toJSON cfg
    Maps.NextBillionConfig cfg -> toJSON cfg
  Domain.SmsServiceConfig smsCfg -> case smsCfg of
    Sms.ExotelSmsConfig cfg -> toJSON cfg
    Sms.MyValueFirstConfig cfg -> toJSON cfg
    Sms.GupShupConfig cfg -> toJSON cfg
    Sms.TwillioSmsConfig cfg -> toJSON cfg
    Sms.DigoEngageSmsConfig cfg -> toJSON cfg
    Sms.VonageSmsConfig cfg -> toJSON cfg
    Sms.KarixSmsConfig cfg -> toJSON cfg
  Domain.WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig cfg -> toJSON cfg
    Whatsapp.TataCommunicationsConfig cfg -> toJSON cfg
  Domain.VerificationServiceConfig verificationCfg -> case verificationCfg of
    Verification.IdfyConfig cfg -> toJSON cfg
    Verification.FaceVerificationConfig cfg -> toJSON cfg
    Verification.GovtDataConfig -> toJSON (A.object [])
    Verification.HyperVergeVerificationConfig cfg -> toJSON cfg
    Verification.HyperVergeVerificationConfigRCDL cfg -> toJSON cfg
    Verification.DigiLockerConfig cfg -> toJSON cfg
  Domain.DriverBackgroundVerificationServiceConfig driverBackgroundVerificationCfg -> case driverBackgroundVerificationCfg of
    Verification.SafetyPortalConfig cfg -> toJSON cfg
  Domain.CallServiceConfig callCfg -> case callCfg of
    Call.ExotelConfig cfg -> toJSON cfg
    Call.TwillioCallConfig cfg -> toJSON cfg
    Call.TataClickToCallConfig cfg -> toJSON cfg
  Domain.AadhaarVerificationServiceConfig aadhaarVerificationCfg -> case aadhaarVerificationCfg of
    AadhaarVerification.GridlineConfig cfg -> toJSON cfg
  Domain.PaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> toJSON cfg
    Payment.StripeConfig cfg -> toJSON cfg
  Domain.PayoutServiceConfig payoutCfg -> case payoutCfg of
    Payout.JuspayConfig cfg -> toJSON cfg
  Domain.RentalPayoutServiceConfig payoutCfg -> case payoutCfg of
    Payout.JuspayConfig cfg -> toJSON cfg
  Domain.RentalPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> toJSON cfg
    Payment.StripeConfig cfg -> toJSON cfg
  Domain.CautioPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> toJSON cfg
    Payment.StripeConfig cfg -> toJSON cfg
  Domain.IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig cfg -> toJSON cfg
  Domain.NotificationServiceConfig notificationServiceCfg -> case notificationServiceCfg of
    Notification.FCMConfig cfg -> toJSON cfg
    Notification.PayTMConfig cfg -> toJSON cfg
    Notification.GRPCConfig cfg -> toJSON cfg
  Domain.TokenizationServiceConfig tokenizationCfg -> case tokenizationCfg of
    Tokenize.HyperVergeTokenizationServiceConfig cfg -> toJSON cfg
    Tokenize.JourneyMonitoringTokenizationServiceConfig cfg -> toJSON cfg
    Tokenize.GullakTokenizationServiceConfig cfg -> toJSON cfg
    Tokenize.DigilockerTokenizationServiceConfig cfg -> toJSON cfg
  Domain.BackgroundVerificationServiceConfig backgroundVerificationCfg -> case backgroundVerificationCfg of
    BackgroundVerification.CheckrConfig cfg -> toJSON cfg
  Domain.IncidentReportServiceConfig incidentReportCfg -> case incidentReportCfg of
    IncidentReport.ERSSConfig cfg -> toJSON cfg
  Domain.LLMChatCompletionServiceConfig chatCompletionCfg -> case chatCompletionCfg of
    CIT.AzureOpenAI cfg -> toJSON cfg
    CIT.Gemini cfg -> toJSON cfg
  Domain.DashCamServiceConfig dashcamCfg -> case dashcamCfg of
    DashcamInter.CautioConfig cfg -> toJSON cfg

getServiceName :: Domain.ServiceConfig -> Domain.ServiceName
getServiceName = \case
  Domain.MapsServiceConfig mapsCfg -> case mapsCfg of
    Maps.GoogleConfig _ -> Domain.MapsService Maps.Google
    Maps.OSRMConfig _ -> Domain.MapsService Maps.OSRM
    Maps.MMIConfig _ -> Domain.MapsService Maps.MMI
    Maps.NextBillionConfig _ -> Domain.MapsService Maps.NextBillion
  Domain.SmsServiceConfig smsCfg -> case smsCfg of
    Sms.ExotelSmsConfig _ -> Domain.SmsService Sms.ExotelSms
    Sms.MyValueFirstConfig _ -> Domain.SmsService Sms.MyValueFirst
    Sms.GupShupConfig _ -> Domain.SmsService Sms.GupShup
    Sms.TwillioSmsConfig _ -> Domain.SmsService Sms.TwillioSms
    Sms.DigoEngageSmsConfig _ -> Domain.SmsService Sms.DigoEngage
    Sms.VonageSmsConfig _ -> Domain.SmsService Sms.VonageSms
    Sms.KarixSmsConfig _ -> Domain.SmsService Sms.KarixSms
  Domain.WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig _ -> Domain.WhatsappService Whatsapp.GupShup
    Whatsapp.TataCommunicationsConfig _ -> Domain.WhatsappService Whatsapp.TataCommunications
  Domain.VerificationServiceConfig verificationCfg -> case verificationCfg of
    Verification.IdfyConfig _ -> Domain.VerificationService Verification.Idfy
    Verification.FaceVerificationConfig _ -> Domain.VerificationService Verification.InternalScripts
    Verification.GovtDataConfig -> Domain.VerificationService Verification.GovtData
    Verification.HyperVergeVerificationConfig _ -> Domain.VerificationService Verification.HyperVerge
    Verification.HyperVergeVerificationConfigRCDL _ -> Domain.VerificationService Verification.HyperVergeRCDL
    Verification.DigiLockerConfig _ -> Domain.VerificationService Verification.DigiLocker
  Domain.DriverBackgroundVerificationServiceConfig driverBackgroundVerificationCfg -> case driverBackgroundVerificationCfg of
    Verification.SafetyPortalConfig _ -> Domain.DriverBackgroundVerificationService Verification.SafetyPortal
  Domain.CallServiceConfig callCfg -> case callCfg of
    Call.ExotelConfig _ -> Domain.CallService Call.Exotel
    Call.TwillioCallConfig _ -> Domain.CallService Call.TwillioCall
    Call.TataClickToCallConfig _ -> Domain.CallService Call.TataClickToCall
  Domain.AadhaarVerificationServiceConfig aadhaarVerificationCfg -> case aadhaarVerificationCfg of
    AadhaarVerification.GridlineConfig _ -> Domain.AadhaarVerificationService AadhaarVerification.Gridline
  Domain.PaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> Domain.PaymentService Payment.AAJuspay
      _ -> Domain.PaymentService Payment.Juspay
    Payment.StripeConfig _ -> Domain.PaymentService Payment.Stripe
  Domain.PayoutServiceConfig payoutCfg -> case payoutCfg of
    Payout.JuspayConfig _ -> Domain.PayoutService Payout.Juspay
  Domain.RentalPayoutServiceConfig payoutCfg -> case payoutCfg of
    Payout.JuspayConfig _ -> Domain.RentalPayoutService Payout.Juspay
  Domain.RentalPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> Domain.RentalPaymentService Payment.AAJuspay
      _ -> Domain.RentalPaymentService Payment.Juspay
    Payment.StripeConfig _ -> Domain.RentalPaymentService Payment.Stripe
  Domain.CautioPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> Domain.RentalPaymentService Payment.AAJuspay
      _ -> Domain.RentalPaymentService Payment.Juspay
    Payment.StripeConfig _ -> Domain.CautioPaymentService Payment.Stripe
  Domain.IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig _ -> Domain.IssueTicketService Ticket.Kapture
  Domain.NotificationServiceConfig notificationServiceCfg -> case notificationServiceCfg of
    Notification.FCMConfig _ -> Domain.NotificationService Notification.FCM
    Notification.PayTMConfig _ -> Domain.NotificationService Notification.PayTM
    Notification.GRPCConfig _ -> Domain.NotificationService Notification.GRPC
  Domain.TokenizationServiceConfig tokenizationConfig -> case tokenizationConfig of
    Tokenize.HyperVergeTokenizationServiceConfig _ -> Domain.TokenizationService Tokenize.HyperVerge
    Tokenize.GullakTokenizationServiceConfig _ -> Domain.TokenizationService Tokenize.Gullak
    Tokenize.JourneyMonitoringTokenizationServiceConfig _ -> Domain.TokenizationService Tokenize.JourneyMonitoring
    Tokenize.DigilockerTokenizationServiceConfig _ -> Domain.TokenizationService Tokenize.Digilocker
  Domain.BackgroundVerificationServiceConfig backgroundVerificationCfg -> case backgroundVerificationCfg of
    BackgroundVerification.CheckrConfig _ -> Domain.BackgroundVerificationService BackgroundVerification.Checkr
  Domain.IncidentReportServiceConfig incidentReportCfg -> case incidentReportCfg of
    IncidentReport.ERSSConfig _ -> Domain.IncidentReportService IncidentReport.ERSS
  Domain.LLMChatCompletionServiceConfig chatCompletionCfg -> case chatCompletionCfg of
    CIT.AzureOpenAI _ -> Domain.LLMChatCompletionService ChatCompletion.Types.AzureOpenAI
    CIT.Gemini _ -> Domain.LLMChatCompletionService ChatCompletion.Types.Gemini
  Domain.DashCamServiceConfig dashcamCfg -> case dashcamCfg of
    DashcamInter.CautioConfig _ -> Domain.DashCamService Dashcam.Cautio

mkServiceConfig :: (MonadThrow m, Log m) => Data.Aeson.Value -> Domain.ServiceName -> m Domain.ServiceConfig
mkServiceConfig configJSON serviceName = either (\err -> throwError $ InternalError ("Unable to decode MerchantServiceConfigT.configJSON: " <> show configJSON <> " Error:" <> err)) return $ case serviceName of
  Domain.MapsService Maps.Google -> Domain.MapsServiceConfig . Maps.GoogleConfig <$> eitherValue configJSON
  Domain.MapsService Maps.OSRM -> Domain.MapsServiceConfig . Maps.OSRMConfig <$> eitherValue configJSON
  Domain.MapsService Maps.MMI -> Domain.MapsServiceConfig . Maps.MMIConfig <$> eitherValue configJSON
  Domain.MapsService Maps.NextBillion -> Domain.MapsServiceConfig . Maps.NextBillionConfig <$> eitherValue configJSON
  Domain.MapsService Maps.SelfTuned -> Left "No Config Found For SelfTuned."
  Domain.SmsService Sms.ExotelSms -> Domain.SmsServiceConfig . Sms.ExotelSmsConfig <$> eitherValue configJSON
  Domain.SmsService Sms.MyValueFirst -> Domain.SmsServiceConfig . Sms.MyValueFirstConfig <$> eitherValue configJSON
  Domain.SmsService Sms.GupShup -> Domain.SmsServiceConfig . Sms.GupShupConfig <$> eitherValue configJSON
  Domain.SmsService Sms.TwillioSms -> Domain.SmsServiceConfig . Sms.TwillioSmsConfig <$> eitherValue configJSON
  Domain.SmsService Sms.DigoEngage -> Domain.SmsServiceConfig . Sms.DigoEngageSmsConfig <$> eitherValue configJSON
  Domain.SmsService Sms.VonageSms -> Domain.SmsServiceConfig . Sms.VonageSmsConfig <$> eitherValue configJSON
  Domain.SmsService Sms.KarixSms -> Domain.SmsServiceConfig . Sms.KarixSmsConfig <$> eitherValue configJSON
  Domain.WhatsappService Whatsapp.GupShup -> Domain.WhatsappServiceConfig . Whatsapp.GupShupConfig <$> eitherValue configJSON
  Domain.WhatsappService Whatsapp.TataCommunications -> Domain.WhatsappServiceConfig . Whatsapp.TataCommunicationsConfig <$> eitherValue configJSON
  Domain.VerificationService Verification.Idfy -> Domain.VerificationServiceConfig . Verification.IdfyConfig <$> eitherValue configJSON
  Domain.VerificationService Verification.InternalScripts -> Domain.VerificationServiceConfig . Verification.FaceVerificationConfig <$> eitherValue configJSON
  Domain.VerificationService Verification.GovtData -> Right $ Domain.VerificationServiceConfig Verification.GovtDataConfig
  Domain.VerificationService Verification.HyperVerge -> Domain.VerificationServiceConfig . Verification.HyperVergeVerificationConfig <$> eitherValue configJSON
  Domain.VerificationService Verification.HyperVergeRCDL -> Domain.VerificationServiceConfig . Verification.HyperVergeVerificationConfigRCDL <$> eitherValue configJSON
  Domain.VerificationService Verification.DigiLocker -> Domain.VerificationServiceConfig . Verification.DigiLockerConfig <$> eitherValue configJSON
  Domain.DriverBackgroundVerificationService Verification.SafetyPortal -> Domain.DriverBackgroundVerificationServiceConfig . Verification.SafetyPortalConfig <$> eitherValue configJSON
  Domain.CallService Call.Exotel -> Domain.CallServiceConfig . Call.ExotelConfig <$> eitherValue configJSON
  Domain.CallService Call.TwillioCall -> Domain.CallServiceConfig . Call.TwillioCallConfig <$> eitherValue configJSON
  Domain.CallService Call.TataClickToCall -> Domain.CallServiceConfig . Call.TataClickToCallConfig <$> eitherValue configJSON
  Domain.CallService Call.Knowlarity -> Left "No Config Found For Knowlarity."
  Domain.AadhaarVerificationService AadhaarVerification.Gridline -> Domain.AadhaarVerificationServiceConfig . AadhaarVerification.GridlineConfig <$> eitherValue configJSON
  Domain.PaymentService Payment.Juspay -> Domain.PaymentServiceConfig . Payment.JuspayConfig <$> eitherValue configJSON
  Domain.PaymentService Payment.AAJuspay -> Domain.PaymentServiceConfig . Payment.JuspayConfig <$> eitherValue configJSON
  Domain.PaymentService Payment.Stripe -> Domain.PaymentServiceConfig . Payment.StripeConfig <$> eitherValue configJSON
  Domain.PayoutService Payout.Juspay -> Domain.PayoutServiceConfig . Payout.JuspayConfig <$> eitherValue configJSON
  Domain.PayoutService Payout.AAJuspay -> Domain.PayoutServiceConfig . Payout.JuspayConfig <$> eitherValue configJSON
  Domain.RentalPayoutService Payout.Juspay -> Domain.RentalPayoutServiceConfig . Payout.JuspayConfig <$> eitherValue configJSON
  Domain.RentalPayoutService Payout.AAJuspay -> Domain.RentalPayoutServiceConfig . Payout.JuspayConfig <$> eitherValue configJSON
  Domain.RentalPaymentService Payment.AAJuspay -> Domain.RentalPaymentServiceConfig . Payment.JuspayConfig <$> eitherValue configJSON
  Domain.RentalPaymentService Payment.Juspay -> Domain.RentalPaymentServiceConfig . Payment.JuspayConfig <$> eitherValue configJSON
  Domain.RentalPaymentService Payment.Stripe -> Domain.RentalPaymentServiceConfig . Payment.StripeConfig <$> eitherValue configJSON
  Domain.CautioPaymentService Payment.Juspay -> Domain.CautioPaymentServiceConfig . Payment.JuspayConfig <$> eitherValue configJSON
  Domain.CautioPaymentService Payment.AAJuspay -> Domain.CautioPaymentServiceConfig . Payment.JuspayConfig <$> eitherValue configJSON
  Domain.CautioPaymentService Payment.Stripe -> Domain.CautioPaymentServiceConfig . Payment.StripeConfig <$> eitherValue configJSON
  Domain.IssueTicketService Ticket.Kapture -> Domain.IssueTicketServiceConfig . Ticket.KaptureConfig <$> eitherValue configJSON
  Domain.NotificationService Notification.FCM -> Domain.NotificationServiceConfig . Notification.FCMConfig <$> eitherValue configJSON
  Domain.NotificationService Notification.PayTM -> Domain.NotificationServiceConfig . Notification.PayTMConfig <$> eitherValue configJSON
  Domain.NotificationService Notification.GRPC -> Domain.NotificationServiceConfig . Notification.GRPCConfig <$> eitherValue configJSON
  Domain.TokenizationService Tokenize.HyperVerge -> Domain.TokenizationServiceConfig . Tokenize.HyperVergeTokenizationServiceConfig <$> eitherValue configJSON
  Domain.TokenizationService Tokenize.JourneyMonitoring -> Domain.TokenizationServiceConfig . Tokenize.JourneyMonitoringTokenizationServiceConfig <$> eitherValue configJSON
  Domain.TokenizationService Tokenize.Gullak -> Domain.TokenizationServiceConfig . Tokenize.GullakTokenizationServiceConfig <$> eitherValue configJSON
  Domain.TokenizationService Tokenize.Digilocker -> Domain.TokenizationServiceConfig . Tokenize.DigilockerTokenizationServiceConfig <$> eitherValue configJSON
  Domain.BackgroundVerificationService BackgroundVerification.Checkr -> Domain.BackgroundVerificationServiceConfig . BackgroundVerification.CheckrConfig <$> eitherValue configJSON
  Domain.IncidentReportService IncidentReport.ERSS -> Domain.IncidentReportServiceConfig . IncidentReport.ERSSConfig <$> eitherValue configJSON
  Domain.LLMChatCompletionService ChatCompletion.Types.AzureOpenAI -> Domain.LLMChatCompletionServiceConfig . CIT.AzureOpenAI <$> eitherValue configJSON
  Domain.LLMChatCompletionService ChatCompletion.Types.Gemini -> Domain.LLMChatCompletionServiceConfig . CIT.Gemini <$> eitherValue configJSON
  Domain.DashCamService Dashcam.Cautio -> Domain.DashCamServiceConfig . DashcamInter.CautioConfig <$> eitherValue configJSON
  where
    eitherValue :: FromJSON a => A.Value -> Either Text a
    eitherValue value = case A.fromJSON value of
      A.Success a -> Right a
      A.Error err -> Left $ T.pack err
