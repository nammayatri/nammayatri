module Storage.Queries.MerchantServiceConfigExtra where

import qualified Data.Aeson as A
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig
import qualified Domain.Types.MerchantServiceConfig as Domain
import Kernel.Beam.Functions
import qualified Kernel.External.AadhaarVerification as AadhaarVerification
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
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Kernel.Utils.Logging
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantServiceConfig as BeamMSC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.OrphanInstances.MerchantServiceConfig ()
import qualified Utils.Common.JWT.Config as GW

-- Extra code goes here --
findByMerchantOpCityIdAndService ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ServiceName ->
  m (Maybe MerchantServiceConfig)
findByMerchantOpCityIdAndService (Id merchantId) (Id merchantOperatingCity) serviceName = do
  resp <- findByMerchantOpCityIdAndService' (Id merchantId) (Id merchantOperatingCity) serviceName
  case resp of
    Just msc -> return $ Just msc
    Nothing -> do
      logError $ show (MerchantServiceConfigNotFound (merchantId <> "mocId" <> merchantOperatingCity) "ServiceName" (show serviceName))
      merchant <- CQM.findById (Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
      merchantOperatingCity' <- CQMOC.findByMerchantShortIdAndCity merchant.shortId merchant.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show merchant.defaultCity)
      resp' <- findByMerchantOpCityIdAndService' (Id merchantId) (merchantOperatingCity'.id) serviceName >>= fromMaybeM (MerchantServiceConfigNotFound (merchantId <> "mocId" <> merchantOperatingCity'.id.getId) "ServiceName" (show serviceName))
      return $ Just resp'

findByMerchantOpCityIdAndService' ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ServiceName ->
  m (Maybe MerchantServiceConfig)
findByMerchantOpCityIdAndService' (Id merchantId) (Id merchantOperatingCity) serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMSC.merchantId $ Se.Eq merchantId,
          Se.Is BeamMSC.merchantOperatingCityId $ Se.Eq merchantOperatingCity,
          Se.Is BeamMSC.serviceName $ Se.Eq serviceName
        ]
    ]

upsertMerchantServiceConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceConfig -> m ()
upsertMerchantServiceConfig merchantServiceConfig = do
  now <- getCurrentTime
  let (_serviceName, configJSON) = getServiceNameConfigJSON merchantServiceConfig.serviceConfig
  res <- findByMerchantOpCityIdAndService merchantServiceConfig.merchantId merchantServiceConfig.merchantOperatingCityId _serviceName
  if isJust res
    then
      updateWithKV
        [Se.Set BeamMSC.configJSON configJSON, Se.Set BeamMSC.updatedAt now]
        [ Se.And
            [ Se.Is BeamMSC.merchantId $ Se.Eq $ getId merchantServiceConfig.merchantId,
              Se.Is BeamMSC.merchantOperatingCityId $ Se.Eq $ getId merchantServiceConfig.merchantOperatingCityId,
              Se.Is BeamMSC.serviceName $ Se.Eq _serviceName
            ]
        ]
    else createWithKV merchantServiceConfig

getServiceNameConfigJSON :: Domain.ServiceConfig -> (Domain.ServiceName, A.Value)
getServiceNameConfigJSON = \case
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
    Payment.JuspayConfig cfg -> (Domain.PaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.PaymentService Payment.Stripe, toJSON cfg)
  Domain.MetroPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> (Domain.MetroPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.MetroPaymentService Payment.Stripe, toJSON cfg)
  Domain.BusPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> (Domain.BusPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.BusPaymentService Payment.Stripe, toJSON cfg)
  Domain.BbpsPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> (Domain.BbpsPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.BbpsPaymentService Payment.Stripe, toJSON cfg)
  Domain.MultiModalPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> (Domain.MultiModalPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.MultiModalPaymentService Payment.Stripe, toJSON cfg)
  Domain.PassPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> (Domain.PassPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.PassPaymentService Payment.Stripe, toJSON cfg)
  Domain.ParkingPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> (Domain.ParkingPaymentService Payment.Juspay, toJSON cfg)
    Payment.StripeConfig cfg -> (Domain.ParkingPaymentService Payment.Stripe, toJSON cfg)
  Domain.IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig cfg -> (Domain.IssueTicketService Ticket.Kapture, toJSON cfg)
  Domain.IncidentReportServiceConfig incidentReportCfg -> case incidentReportCfg of
    IncidentReport.ERSSConfig cfg -> (Domain.IncidentReportService IncidentReport.ERSS, toJSON cfg)
  Domain.TokenizationServiceConfig tokenizationCfg -> case tokenizationCfg of
    Tokenize.JourneyMonitoringTokenizationServiceConfig cfg -> (Domain.TokenizationService Tokenize.JourneyMonitoring, toJSON cfg)
    Tokenize.GullakTokenizationServiceConfig cfg -> (Domain.TokenizationService Tokenize.Gullak, toJSON cfg)
    Tokenize.HyperVergeTokenizationServiceConfig cfg -> (Domain.TokenizationService Tokenize.HyperVerge, toJSON cfg)
    Tokenize.DigilockerTokenizationServiceConfig cfg -> (Domain.TokenizationService Tokenize.Digilocker, toJSON cfg)
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
