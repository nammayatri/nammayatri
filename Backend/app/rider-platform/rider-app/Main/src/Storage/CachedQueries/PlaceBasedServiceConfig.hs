{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.PlaceBasedServiceConfig
  ( findByPlaceIdAndServiceName,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.MerchantServiceConfig
import Domain.Types.PlaceBasedServiceConfig
import Domain.Types.TicketPlace
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
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.External.SMS.Interface as Sms
import Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Tokenize as Tokenize
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PlaceBasedServiceConfig as Queries
import Utils.Common.JWT.Config as GW

findByPlaceIdAndServiceName :: (CacheFlow m r, EsqDBFlow m r) => Id TicketPlace -> ServiceName -> m (Maybe PlaceBasedServiceConfig)
findByPlaceIdAndServiceName id serviceName =
  Hedis.safeGet (makePlaceIdAndServiceKey id serviceName) >>= \case
    Just a -> return . Just $ coerce @(PlaceBasedServiceConfigD 'Unsafe) @PlaceBasedServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByPlaceIdAndServiceName id serviceName

cacheMerchantServiceConfig :: CacheFlow m r => PlaceBasedServiceConfig -> m ()
cacheMerchantServiceConfig placeBasedServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makePlaceIdAndServiceKey placeBasedServiceConfig.placeId (getServiceNameFromPlaceBasedConfigs placeBasedServiceConfig)
  Hedis.setExp idKey (coerce @PlaceBasedServiceConfig @(PlaceBasedServiceConfigD 'Unsafe) placeBasedServiceConfig) expTime

makePlaceIdAndServiceKey :: Id TicketPlace -> ServiceName -> Text
makePlaceIdAndServiceKey id serviceName = "CachedQueries:PlaceBasedServiceConfig:PlaceId-" <> id.getId <> ":ServiceName-" <> show serviceName

getServiceNameFromPlaceBasedConfigs :: PlaceBasedServiceConfig -> ServiceName
getServiceNameFromPlaceBasedConfigs msc = case msc.serviceConfig of
  MapsServiceConfig mapsCfg -> case mapsCfg of
    Maps.GoogleConfig _ -> MapsService Maps.Google
    Maps.OSRMConfig _ -> MapsService Maps.OSRM
    Maps.MMIConfig _ -> MapsService Maps.MMI
    Maps.NextBillionConfig _ -> MapsService Maps.NextBillion
  SmsServiceConfig smsCfg -> case smsCfg of
    Sms.ExotelSmsConfig _ -> SmsService Sms.ExotelSms
    Sms.MyValueFirstConfig _ -> SmsService Sms.MyValueFirst
    Sms.GupShupConfig _ -> SmsService Sms.GupShup
    Sms.TwillioSmsConfig _ -> SmsService Sms.TwillioSms
    Sms.DigoEngageSmsConfig _ -> SmsService Sms.DigoEngage
    Sms.VonageSmsConfig _ -> SmsService Sms.VonageSms
    Sms.KarixSmsConfig _ -> SmsService Sms.KarixSms
    Sms.PinbixSmsConfig _ -> SmsService Sms.PinbixSms
  WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig _ -> WhatsappService Whatsapp.GupShup
    Whatsapp.TataCommunicationsConfig _ -> WhatsappService Whatsapp.TataCommunications
    Whatsapp.KarixConfig _ -> WhatsappService Whatsapp.Karix
  AadhaarVerificationServiceConfig aadhaarVerifictaionCfg -> case aadhaarVerifictaionCfg of
    AadhaarVerification.GridlineConfig _ -> AadhaarVerificationService AadhaarVerification.Gridline
  CallServiceConfig callCfg -> case callCfg of
    Call.ExotelConfig _ -> CallService Call.Exotel
    Call.TwillioCallConfig _ -> CallService Call.TwillioCall
    Call.TataClickToCallConfig _ -> CallService Call.TataClickToCall
  NotificationServiceConfig notificationCfg -> case notificationCfg of
    Notification.FCMConfig _ -> NotificationService Notification.FCM
    Notification.PayTMConfig _ -> NotificationService Notification.PayTM
    Notification.GRPCConfig _ -> NotificationService Notification.GRPC
  PaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> PaymentService Payment.Juspay
    Payment.StripeConfig _ -> PaymentService Payment.Stripe
  MetroPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> MetroPaymentService Payment.Juspay
    Payment.StripeConfig _ -> MetroPaymentService Payment.Stripe
  BusPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> BusPaymentService Payment.Juspay
    Payment.StripeConfig _ -> BusPaymentService Payment.Stripe
  BbpsPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> BbpsPaymentService Payment.Juspay
    Payment.StripeConfig _ -> BbpsPaymentService Payment.Stripe
  MultiModalPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> MultiModalPaymentService Payment.Juspay
    Payment.StripeConfig _ -> MultiModalPaymentService Payment.Stripe
  PassPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> PassPaymentService Payment.Juspay
    Payment.StripeConfig _ -> PassPaymentService Payment.Stripe
  ParkingPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> ParkingPaymentService Payment.Juspay
    Payment.StripeConfig _ -> ParkingPaymentService Payment.Stripe
  IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig _ -> IssueTicketService Ticket.Kapture
  IncidentReportServiceConfig incidentReportCfg -> case incidentReportCfg of
    IncidentReport.ERSSConfig _ -> IncidentReportService IncidentReport.ERSS
  TokenizationServiceConfig tokenizeCfg -> case tokenizeCfg of
    Tokenize.JourneyMonitoringTokenizationServiceConfig _ -> TokenizationService Tokenize.JourneyMonitoring
    Tokenize.GullakTokenizationServiceConfig _ -> TokenizationService Tokenize.Gullak
    Tokenize.HyperVergeTokenizationServiceConfig _ -> TokenizationService Tokenize.HyperVerge
    Tokenize.DigilockerTokenizationServiceConfig _ -> TokenizationService Tokenize.Digilocker
  PayoutServiceConfig payoutCfg -> case payoutCfg of
    Payout.JuspayConfig _ -> PayoutService Payout.Juspay
  MultiModalServiceConfig multiModalCfg -> case multiModalCfg of
    MultiModal.GoogleTransitConfig _ -> MultiModalService MultiModal.GoogleTransit
    MultiModal.OTPTransitConfig _ -> MultiModalService MultiModal.OTPTransit
  WalletServiceConfig walletCfg -> case walletCfg of
    GW.GoogleWalletConfig _ -> WalletService GW.GoogleWallet
  JuspayWalletServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> JuspayWalletService Payment.Juspay
    Payment.StripeConfig _ -> JuspayWalletService Payment.Stripe
  MultiModalStaticDataServiceConfig multiModalStaticDataCfg -> case multiModalStaticDataCfg of
    MultiModal.GoogleTransitConfig _ -> MultiModalStaticDataService MultiModal.GoogleTransit
    MultiModal.OTPTransitConfig _ -> MultiModalStaticDataService MultiModal.OTPTransit
  InsuranceServiceConfig insuranceCfg -> case insuranceCfg of
    Insurance.AckoInsuranceConfig _ -> InsuranceService Insurance.Acko
