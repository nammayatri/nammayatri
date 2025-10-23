module Storage.Queries.Transformers.MerchantServiceConfig where

import qualified Data.Aeson as A
import qualified Domain.Types.MerchantServiceConfig as Domain
import qualified Kernel.External.AadhaarVerification.Interface as AadhaarVerification
import qualified Kernel.External.Call as Call
import Kernel.External.IncidentReport.Interface.Types as IncidentReport
import qualified Kernel.External.Insurance.Interface.Types as Insurance
import qualified Kernel.External.Insurance.Types as Insurance
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import Kernel.External.MultiModal.Interface.Types as MultiModal
import Kernel.External.MultiModal.Types as MultiModal
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types as Notification
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.External.SMS.Interface as Sms
import Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Tokenize as Tokenize
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.JSON (valueToMaybe)
import qualified Utils.Common.JWT.Config as GW

getServiceConfigFromDomain :: (MonadFlow m) => Domain.ServiceName -> A.Value -> m Domain.ServiceConfig
getServiceConfigFromDomain serviceName configJSON = do
  maybe (throwError $ InternalError ("Unable to decode MerchantServiceConfigT.configJSON - " <> show serviceName <> " | " <> encodeToText configJSON)) return $ case serviceName of
    Domain.MapsService Maps.Google -> Domain.MapsServiceConfig . Maps.GoogleConfig <$> valueToMaybe configJSON
    Domain.MapsService Maps.OSRM -> Domain.MapsServiceConfig . Maps.OSRMConfig <$> valueToMaybe configJSON
    Domain.MapsService Maps.MMI -> Domain.MapsServiceConfig . Maps.MMIConfig <$> valueToMaybe configJSON
    Domain.MapsService Maps.NextBillion -> Domain.MapsServiceConfig . Maps.NextBillionConfig <$> valueToMaybe configJSON
    Domain.MapsService Maps.SelfTuned -> Nothing
    Domain.SmsService Sms.ExotelSms -> Domain.SmsServiceConfig . Sms.ExotelSmsConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.MyValueFirst -> Domain.SmsServiceConfig . Sms.MyValueFirstConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.GupShup -> Domain.SmsServiceConfig . Sms.GupShupConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.TwillioSms -> Domain.SmsServiceConfig . Sms.TwillioSmsConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.DigoEngage -> Domain.SmsServiceConfig . Sms.DigoEngageSmsConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.KarixSms -> Domain.SmsServiceConfig . Sms.KarixSmsConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.VonageSms -> Domain.SmsServiceConfig . Sms.VonageSmsConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.KarixSms -> Domain.SmsServiceConfig . Sms.KarixSmsConfig <$> valueToMaybe configJSON
    Domain.WhatsappService Whatsapp.GupShup -> Domain.WhatsappServiceConfig . Whatsapp.GupShupConfig <$> valueToMaybe configJSON
    Domain.WhatsappService Whatsapp.TataCommunications -> Domain.WhatsappServiceConfig . Whatsapp.TataCommunicationsConfig <$> valueToMaybe configJSON
    Domain.CallService Call.Exotel -> Domain.CallServiceConfig . Call.ExotelConfig <$> valueToMaybe configJSON
    Domain.CallService Call.TwillioCall -> Domain.CallServiceConfig . Call.TwillioCallConfig <$> valueToMaybe configJSON
    Domain.CallService Call.TataClickToCall -> Domain.CallServiceConfig . Call.TataClickToCallConfig <$> valueToMaybe configJSON
    Domain.CallService Call.Knowlarity -> Nothing
    Domain.AadhaarVerificationService AadhaarVerification.Gridline -> Domain.AadhaarVerificationServiceConfig . AadhaarVerification.GridlineConfig <$> valueToMaybe configJSON
    Domain.NotificationService Notification.FCM -> Domain.NotificationServiceConfig . Notification.FCMConfig <$> valueToMaybe configJSON
    Domain.NotificationService Notification.PayTM -> Domain.NotificationServiceConfig . Notification.PayTMConfig <$> valueToMaybe configJSON
    Domain.NotificationService Notification.GRPC -> Domain.NotificationServiceConfig . Notification.GRPCConfig <$> valueToMaybe configJSON
    Domain.PaymentService Payment.Juspay -> Domain.PaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.PaymentService Payment.AAJuspay -> Domain.PaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.PaymentService Payment.Stripe -> Domain.PaymentServiceConfig . Payment.StripeConfig <$> valueToMaybe configJSON
    Domain.MetroPaymentService Payment.Juspay -> Domain.MetroPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.MetroPaymentService Payment.AAJuspay -> Domain.MetroPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.MetroPaymentService Payment.Stripe -> Domain.MetroPaymentServiceConfig . Payment.StripeConfig <$> valueToMaybe configJSON
    Domain.BusPaymentService Payment.Juspay -> Domain.BusPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.BusPaymentService Payment.AAJuspay -> Domain.BusPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.BusPaymentService Payment.Stripe -> Domain.BusPaymentServiceConfig . Payment.StripeConfig <$> valueToMaybe configJSON
    Domain.BbpsPaymentService Payment.Juspay -> Domain.BbpsPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.BbpsPaymentService Payment.AAJuspay -> Domain.BbpsPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.BbpsPaymentService Payment.Stripe -> Domain.BbpsPaymentServiceConfig . Payment.StripeConfig <$> valueToMaybe configJSON
    Domain.MultiModalPaymentService Payment.Juspay -> Domain.MultiModalPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.MultiModalPaymentService Payment.AAJuspay -> Domain.MultiModalPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.MultiModalPaymentService Payment.Stripe -> Domain.MultiModalPaymentServiceConfig . Payment.StripeConfig <$> valueToMaybe configJSON
    Domain.PassPaymentService Payment.Juspay -> Domain.PassPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.PassPaymentService Payment.AAJuspay -> Domain.PassPaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.PassPaymentService Payment.Stripe -> Domain.PassPaymentServiceConfig . Payment.StripeConfig <$> valueToMaybe configJSON
    Domain.IssueTicketService Ticket.Kapture -> Domain.IssueTicketServiceConfig . Ticket.KaptureConfig <$> valueToMaybe configJSON
    Domain.TokenizationService Tokenize.JourneyMonitoring -> Domain.TokenizationServiceConfig . Tokenize.JourneyMonitoringTokenizationServiceConfig <$> valueToMaybe configJSON
    Domain.TokenizationService Tokenize.HyperVerge -> Domain.TokenizationServiceConfig . Tokenize.HyperVergeTokenizationServiceConfig <$> valueToMaybe configJSON
    Domain.TokenizationService Tokenize.Gullak -> Domain.TokenizationServiceConfig . Tokenize.GullakTokenizationServiceConfig <$> valueToMaybe configJSON
    Domain.IncidentReportService IncidentReport.ERSS -> Domain.IncidentReportServiceConfig . IncidentReport.ERSSConfig <$> valueToMaybe configJSON
    Domain.PayoutService Payout.Juspay -> Domain.PayoutServiceConfig . Payout.JuspayConfig <$> valueToMaybe configJSON
    Domain.PayoutService Payout.AAJuspay -> Domain.PayoutServiceConfig . Payout.JuspayConfig <$> valueToMaybe configJSON
    Domain.MultiModalService MultiModal.GoogleTransit -> Domain.MultiModalServiceConfig . MultiModal.GoogleTransitConfig <$> valueToMaybe configJSON
    Domain.MultiModalService MultiModal.OTPTransit -> Domain.MultiModalServiceConfig . MultiModal.OTPTransitConfig <$> valueToMaybe configJSON
    Domain.WalletService GW.GoogleWallet -> Domain.WalletServiceConfig . GW.GoogleWalletConfig <$> valueToMaybe configJSON
    Domain.MultiModalStaticDataService MultiModal.GoogleTransit -> Domain.MultiModalStaticDataServiceConfig . MultiModal.GoogleTransitConfig <$> valueToMaybe configJSON
    Domain.MultiModalStaticDataService MultiModal.OTPTransit -> Domain.MultiModalStaticDataServiceConfig . MultiModal.OTPTransitConfig <$> valueToMaybe configJSON
    Domain.InsuranceService Insurance.Acko -> Domain.InsuranceServiceConfig . Insurance.AckoInsuranceConfig <$> valueToMaybe configJSON

getServiceNameConfigJson :: Domain.ServiceConfig -> (Domain.ServiceName, A.Value)
getServiceNameConfigJson = \case
  Domain.MapsServiceConfig mapsCfg -> case mapsCfg of
    Maps.GoogleConfig cfg -> (Domain.MapsService Maps.Google, toJSON cfg)
    Maps.OSRMConfig cfg -> (Domain.MapsService Maps.OSRM, toJSON cfg)
    Maps.MMIConfig cfg -> (Domain.MapsService Maps.MMI, toJSON cfg)
    Maps.NextBillionConfig cfg -> (Domain.MapsService Maps.NextBillion, toJSON cfg)
  Domain.SmsServiceConfig smsCfg -> case smsCfg of
    Sms.ExotelSmsConfig cfg -> (Domain.SmsService Sms.ExotelSms, toJSON cfg)
    Sms.MyValueFirstConfig cfg -> (Domain.SmsService Sms.MyValueFirst, toJSON cfg)
    Sms.GupShupConfig cfg -> (Domain.SmsService Sms.GupShup, toJSON cfg)
    Sms.TwillioSmsConfig cfg -> (Domain.SmsService Sms.TwillioSms, toJSON cfg)
    Sms.DigoEngageSmsConfig cfg -> (Domain.SmsService Sms.DigoEngage, toJSON cfg)
    Sms.VonageSmsConfig cfg -> (Domain.SmsService Sms.VonageSms, toJSON cfg)
    Sms.KarixSmsConfig cfg -> (Domain.SmsService Sms.KarixSms, toJSON cfg)
  Domain.WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig cfg -> (Domain.WhatsappService Whatsapp.GupShup, toJSON cfg)
    Whatsapp.TataCommunicationsConfig cfg -> (Domain.WhatsappService Whatsapp.TataCommunications, toJSON cfg)
  Domain.CallServiceConfig callCfg -> case callCfg of
    Call.ExotelConfig cfg -> (Domain.CallService Call.Exotel, toJSON cfg)
    Call.TwillioCallConfig cfg -> (Domain.CallService Call.TwillioCall, toJSON cfg)
    Call.TataClickToCallConfig cfg -> (Domain.CallService Call.TataClickToCall, toJSON cfg)
  Domain.NotificationServiceConfig notificationCfg -> case notificationCfg of
    Notification.FCMConfig cfg -> (Domain.NotificationService Notification.FCM, toJSON cfg)
    Notification.PayTMConfig cfg -> (Domain.NotificationService Notification.PayTM, toJSON cfg)
    Notification.GRPCConfig cfg -> (Domain.NotificationService Notification.GRPC, toJSON cfg)
  Domain.AadhaarVerificationServiceConfig aadhaarVerificationCfg -> case aadhaarVerificationCfg of
    AadhaarVerification.GridlineConfig cfg -> (Domain.AadhaarVerificationService AadhaarVerification.Gridline, toJSON cfg)
  Domain.PaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> (Domain.PaymentService Payment.AAJuspay, toJSON cfg)
      _ -> (Domain.PaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.PaymentService Payment.Juspay, toJSON cfg)
  Domain.MetroPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> (Domain.MetroPaymentService Payment.AAJuspay, toJSON cfg)
      _ -> (Domain.MetroPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.MetroPaymentService Payment.Juspay, toJSON cfg)
  Domain.BusPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> (Domain.BusPaymentService Payment.AAJuspay, toJSON cfg)
      _ -> (Domain.BusPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.BusPaymentService Payment.Juspay, toJSON cfg)
  Domain.BbpsPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> (Domain.BbpsPaymentService Payment.AAJuspay, toJSON cfg)
      _ -> (Domain.BbpsPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.BbpsPaymentService Payment.Juspay, toJSON cfg)
  Domain.MultiModalPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> (Domain.MultiModalPaymentService Payment.AAJuspay, toJSON cfg)
      _ -> (Domain.MultiModalPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.MultiModalPaymentService Payment.Juspay, toJSON cfg)
  Domain.PassPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> case cfg.serviceMode of
      Just Juspay.AA -> (Domain.PassPaymentService Payment.AAJuspay, toJSON cfg)
      _ -> (Domain.PassPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.PassPaymentService Payment.Juspay, toJSON cfg)
  Domain.IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig cfg -> (Domain.IssueTicketService Ticket.Kapture, toJSON cfg)
  Domain.TokenizationServiceConfig tokenizationCfg -> case tokenizationCfg of
    Tokenize.HyperVergeTokenizationServiceConfig cfg -> (Domain.TokenizationService Tokenize.HyperVerge, toJSON cfg)
    Tokenize.GullakTokenizationServiceConfig cfg -> (Domain.TokenizationService Tokenize.Gullak, toJSON cfg)
    Tokenize.JourneyMonitoringTokenizationServiceConfig cfg -> (Domain.TokenizationService Tokenize.JourneyMonitoring, toJSON cfg)
  Domain.IncidentReportServiceConfig incidentReportCfg -> case incidentReportCfg of
    IncidentReport.ERSSConfig cfg -> (Domain.IncidentReportService IncidentReport.ERSS, toJSON cfg)
  Domain.PayoutServiceConfig payoutCfg -> case payoutCfg of
    Payout.JuspayConfig cfg -> (Domain.PayoutService Payout.Juspay, toJSON cfg)
  Domain.MultiModalServiceConfig multiModalCfg -> case multiModalCfg of
    MultiModal.GoogleTransitConfig cfg -> (Domain.MultiModalService MultiModal.GoogleTransit, toJSON cfg)
    MultiModal.OTPTransitConfig cfg -> (Domain.MultiModalService MultiModal.OTPTransit, toJSON cfg)
  Domain.WalletServiceConfig walletCfg -> case walletCfg of
    GW.GoogleWalletConfig cfg -> (Domain.WalletService GW.GoogleWallet, toJSON cfg)
  Domain.MultiModalStaticDataServiceConfig multiModalStaticDataCfg -> case multiModalStaticDataCfg of
    MultiModal.GoogleTransitConfig cfg -> (Domain.MultiModalStaticDataService MultiModal.GoogleTransit, toJSON cfg)
    MultiModal.OTPTransitConfig cfg -> (Domain.MultiModalStaticDataService MultiModal.OTPTransit, toJSON cfg)
  Domain.InsuranceServiceConfig insuranceCfg -> case insuranceCfg of
    Insurance.AckoInsuranceConfig cfg -> (Domain.InsuranceService Insurance.Acko, toJSON cfg)
