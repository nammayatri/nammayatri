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
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types as Notification
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.SMS.Interface as Sms
import Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PlaceBasedServiceConfig as Queries

findByPlaceIdAndServiceName :: KvDbFlow m r => Id TicketPlace -> ServiceName -> m (Maybe PlaceBasedServiceConfig)
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
  WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig _ -> WhatsappService Whatsapp.GupShup
  AadhaarVerificationServiceConfig aadhaarVerifictaionCfg -> case aadhaarVerifictaionCfg of
    AadhaarVerification.GridlineConfig _ -> AadhaarVerificationService AadhaarVerification.Gridline
  CallServiceConfig callCfg -> case callCfg of
    Call.ExotelConfig _ -> CallService Call.Exotel
  NotificationServiceConfig notificationCfg -> case notificationCfg of
    Notification.FCMConfig _ -> NotificationService Notification.FCM
    Notification.PayTMConfig _ -> NotificationService Notification.PayTM
    Notification.GRPCConfig _ -> NotificationService Notification.GRPC
  PaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> PaymentService Payment.Juspay
  MetroPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> MetroPaymentService Payment.Juspay
  IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig _ -> IssueTicketService Ticket.Kapture
