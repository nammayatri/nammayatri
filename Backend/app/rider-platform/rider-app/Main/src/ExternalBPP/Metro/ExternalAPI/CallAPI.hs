module ExternalBPP.Metro.ExternalAPI.CallAPI where

import Domain.Types.IntegratedBPPConfig
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.BusinessHour as CMRLBusinessHour
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.DurationDetails as CMRLDurationDetails
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.FareByOriginDest as CMRLFareByOriginDest
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.FareMatrix as CMRLFareMatrix
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.PassengerViewStatus as CMRLPassengerViewStatus
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.QR as CMRLQR
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.StationList as CMRLStationList
import qualified ExternalBPP.Metro.ExternalAPI.CMRL.TicketStatus as CMRLTicketStatus
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r) => ProviderConfig -> m CMRLBusinessHour.BusinessHourResult
getBusinessHour config = do
  case config of
    CMRL config' -> CMRLBusinessHour.getBusinessHour config'
    _ -> throwError $ InternalError "Unimplemented!"

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLDurationDetails.DurationDetailsReq -> m [CMRLDurationDetails.DurationDetailsResult]
getDurationDetails config req = do
  case config of
    CMRL config' -> CMRLDurationDetails.getDurationDetails config' req
    _ -> throwError $ InternalError "Unimplemented!"

getFareByOriginDest :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLFareByOriginDest.FareByOriginDestReq -> m (Maybe HighPrecMoney)
getFareByOriginDest config fareReq = do
  case config of
    CMRL config' -> CMRLFareByOriginDest.getFareByOriginDest config' fareReq
    _ -> throwError $ InternalError "Unimplemented!"

getFareMatrix :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> m [CMRLFareMatrix.FareMatrixRes]
getFareMatrix config = do
  case config of
    CMRL config' -> CMRLFareMatrix.getFareMatrix config'
    _ -> throwError $ InternalError "Unimplemented!"

generateQRTickets :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLQR.GenerateQRReq -> m [CMRLQR.TicketInfo]
generateQRTickets config qrReq = do
  case config of
    CMRL config' -> CMRLQR.generateQRTickets config' qrReq
    _ -> throwError $ InternalError "Unimplemented!"

getPassengerViewStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLPassengerViewStatus.PassengerViewStatusReq -> m [CMRLPassengerViewStatus.TicketDetails]
getPassengerViewStatus config req = do
  case config of
    CMRL config' -> CMRLPassengerViewStatus.getPassengerViewStatus config' req
    _ -> throwError $ InternalError "Unimplemented!"

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> m [CMRLStationList.Station]
getStationList config = do
  case config of
    CMRL config' -> CMRLStationList.getStationList config'
    _ -> throwError $ InternalError "Unimplemented!"

getTicketStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLTicketStatus.TicketStatusReq -> m CMRLTicketStatus.TicketStatusResult
getTicketStatus config req = do
  case config of
    CMRL config' -> CMRLTicketStatus.getTicketStatus config' req
    _ -> throwError $ InternalError "Unimplemented!"
