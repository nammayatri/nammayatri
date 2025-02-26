module ExternalBPP.ExternalAPI.CallAPI where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified ExternalBPP.ExternalAPI.Bus.EBIX.Order as EBIXOrder
import qualified ExternalBPP.ExternalAPI.Bus.EBIX.Status as EBIXStatus
import qualified ExternalBPP.ExternalAPI.Direct.Order as DIRECTOrder
import qualified ExternalBPP.ExternalAPI.Direct.Status as DIRECTStatus
import qualified ExternalBPP.ExternalAPI.Direct.Verify as DIRECTVerify
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.BusinessHour as CMRLBusinessHour
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.DurationDetails as CMRLDurationDetails
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.FareByOriginDest as CMRLFareByOriginDest
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.FareMatrix as CMRLFareMatrix
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.Order as CMRLOrder
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.PassengerViewStatus as CMRLPassengerViewStatus
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.TicketStatus as CMRLStatus
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSUtils as FRFSUtils
import Tools.Error

getProviderName :: IntegratedBPPConfig -> Text
getProviderName integrationBPPConfig =
  case integrationBPPConfig.providerConfig of
    CMRL _ -> "Chennai Metro Rail Limited"
    EBIX _ -> "Kolkata Buses"
    DIRECT _ -> "Direct Multimodal Services"
    Domain.Types.IntegratedBPPConfig.ONDC _ -> "ONDC Services"

getFares :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r) => Maybe (Id Person) -> Merchant -> MerchantOperatingCity -> IntegratedBPPConfig -> Text -> Text -> Text -> Spec.VehicleCategory -> m [FRFSUtils.FRFSFare]
getFares mbRiderId merchant merchanOperatingCity integrationBPPConfig routeCode startStopCode endStopCode vehicleCategory = do
  case integrationBPPConfig.providerConfig of
    CMRL config' ->
      CMRLFareByOriginDest.getFareByOriginDest config' $
        CMRLFareByOriginDest.FareByOriginDestReq
          { route = routeCode,
            origin = startStopCode,
            destination = endStopCode,
            ticketType = "SJT"
          }
    EBIX _ -> FRFSUtils.getFares mbRiderId vehicleCategory integrationBPPConfig.id merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
    DIRECT _ -> FRFSUtils.getFares mbRiderId vehicleCategory integrationBPPConfig.id merchant.id merchanOperatingCity.id routeCode startStopCode endStopCode
    _ -> throwError $ InternalError "Unimplemented!"

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> Seconds -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m ProviderOrder
createOrder integrationBPPConfig qrTtl (_mRiderName, mRiderNumber) booking = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLOrder.createOrder config' booking mRiderNumber
    EBIX config' -> EBIXOrder.createOrder config' integrationBPPConfig.id qrTtl booking
    DIRECT config' -> DIRECTOrder.createOrder config' integrationBPPConfig.id qrTtl booking
    _ -> throwError $ InternalError "Unimplemented!"

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus integrationBPPConfig booking = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStatus.getTicketStatus config' booking
    EBIX config' -> EBIXStatus.getTicketStatus config' booking
    DIRECT config' -> DIRECTStatus.getTicketStatus config' booking
    _ -> throwError $ InternalError "Unimplemented!"

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => IntegratedBPPConfig -> Text -> m TicketPayload
verifyTicket integrationBPPConfig encryptedQrData = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DIRECTVerify.verifyTicket config' encryptedQrData
    _ -> throwError $ InternalError "Unimplemented!"

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> m CMRLBusinessHour.BusinessHourResult
getBusinessHour integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLBusinessHour.getBusinessHour config'
    _ -> throwError $ InternalError "Unimplemented!"

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> CMRLDurationDetails.DurationDetailsReq -> m [CMRLDurationDetails.DurationDetailsResult]
getDurationDetails integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLDurationDetails.getDurationDetails config' req
    _ -> throwError $ InternalError "Unimplemented!"

getFareMatrix :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> m [CMRLFareMatrix.FareMatrixRes]
getFareMatrix integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLFareMatrix.getFareMatrix config'
    _ -> throwError $ InternalError "Unimplemented!"

getPassengerViewStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> CMRLPassengerViewStatus.PassengerViewStatusReq -> m [CMRLPassengerViewStatus.TicketDetails]
getPassengerViewStatus integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLPassengerViewStatus.getPassengerViewStatus config' req
    _ -> throwError $ InternalError "Unimplemented!"

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => IntegratedBPPConfig -> m [CMRLStationList.Station]
getStationList integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStationList.getStationList config'
    _ -> throwError $ InternalError "Unimplemented!"

getPaymentDetails :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "Unimplemented!"
