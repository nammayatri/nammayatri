{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceConfig
  ( findByMerchantOpCityIdAndService,
    clearCache,
    cacheMerchantServiceConfig,
    upsertMerchantServiceConfig,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig
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
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantServiceConfig as Queries
import qualified Utils.Common.JWT.Config as GW

-- create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceConfig -> m ()
-- create = Queries.create

-- findAllMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [MerchantServiceConfig]
-- findAllMerchantOpCityId id =
--   Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
--     Just a -> return $ fmap (coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig) a
--     Nothing -> cacheMerchantServiceConfigForCity id /=<< Queries.findAllMerchantOpCityId id

-- cacheMerchantServiceConfigForCity :: CacheFlow m r => Id MerchantOperatingCity -> [MerchantServiceConfig] -> m ()
-- cacheMerchantServiceConfigForCity merchantOperatingCityId cfg = do
--   expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
--   let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
--   Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (fmap (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe)) cfg) expTime

-- makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
-- makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:MerchantServiceConfig:MerchantOperatingCityId-" <> id.getId

findByMerchantOpCityIdAndService :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantOpCityIdAndService id mocId serviceName =
  Hedis.safeGet (makeMerchantIdAndServiceKey id mocId serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByMerchantOpCityIdAndService id mocId serviceName

cacheMerchantServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheMerchantServiceConfig merchantServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdAndServiceKey merchantServiceConfig.merchantId merchantServiceConfig.merchantOperatingCityId (getServiceName merchantServiceConfig)
  Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) merchantServiceConfig) expTime
  where
    getServiceName :: MerchantServiceConfig -> ServiceName
    getServiceName msc = case msc.serviceConfig of
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
      TokenizationServiceConfig tokenizationCfg -> case tokenizationCfg of
        Tokenize.JourneyMonitoringTokenizationServiceConfig _ -> TokenizationService Tokenize.JourneyMonitoring
        Tokenize.HyperVergeTokenizationServiceConfig _ -> TokenizationService Tokenize.HyperVerge
        Tokenize.GullakTokenizationServiceConfig _ -> TokenizationService Tokenize.Gullak
        Tokenize.DigilockerTokenizationServiceConfig _ -> TokenizationService Tokenize.Digilocker
      PayoutServiceConfig payoutCfg -> case payoutCfg of
        Payout.JuspayConfig _ -> PayoutService Payout.Juspay
      MultiModalServiceConfig multiModalCfg -> case multiModalCfg of
        MultiModal.GoogleTransitConfig _ -> MultiModalService MultiModal.GoogleTransit
        MultiModal.OTPTransitConfig _ -> MultiModalService MultiModal.OTPTransit
      WalletServiceConfig walletCfg -> case walletCfg of
        GW.GoogleWalletConfig _ -> WalletService GW.GoogleWallet
      MultiModalStaticDataServiceConfig multiModalStaticDataCfg -> case multiModalStaticDataCfg of
        MultiModal.GoogleTransitConfig _ -> MultiModalStaticDataService MultiModal.GoogleTransit
        MultiModal.OTPTransitConfig _ -> MultiModalStaticDataService MultiModal.OTPTransit
      InsuranceServiceConfig insuranceCfg -> case insuranceCfg of
        Insurance.AckoInsuranceConfig _ -> InsuranceService Insurance.Acko

makeMerchantIdAndServiceKey :: Id Merchant -> Id DMOC.MerchantOperatingCity -> ServiceName -> Text
makeMerchantIdAndServiceKey id mocId serviceName = "CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":MechantOperatingCityId:-" <> mocId.getId <> ":ServiceName-" <> show serviceName

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> Id DMOC.MerchantOperatingCity -> ServiceName -> m ()
clearCache merchantId mocId serviceName = do
  Hedis.del (makeMerchantIdAndServiceKey merchantId mocId serviceName)

upsertMerchantServiceConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceConfig -> m ()
upsertMerchantServiceConfig = Queries.upsertMerchantServiceConfig
