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

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r) => ProviderConfig -> m CMRLBusinessHour.BusinessHourResult
getBusinessHour config = do
  case config of
    CMRL config' -> CMRLBusinessHour.getBusinessHour config'
    _ -> error "Unimplemented!"

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLDurationDetails.DurationDetailsReq -> m [CMRLDurationDetails.DurationDetailsResult]
getDurationDetails config req = do
  case config of
    CMRL config' -> CMRLDurationDetails.getDurationDetails config' req
    _ -> error "Unimplemented!"

getFareByOriginDest :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLFareByOriginDest.FareByOriginDestReq -> m HighPrecMoney
getFareByOriginDest config fareReq = do
  case config of
    CMRL config' -> CMRLFareByOriginDest.getFareByOriginDest config' fareReq
    _ -> error "Unimplemented!"

getFareMatrix :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> m [CMRLFareMatrix.FareMatrixRes]
getFareMatrix config = do
  case config of
    CMRL config' -> CMRLFareMatrix.getFareMatrix config'
    _ -> error "Unimplemented!"

generateQRTickets :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLQR.GenerateQRReq -> m [CMRLQR.TicketInfo]
generateQRTickets config qrReq = do
  case config of
    CMRL config' -> CMRLQR.generateQRTickets config' qrReq
    _ -> error "Unimplemented!"

getPassengerViewStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLPassengerViewStatus.PassengerViewStatusReq -> m [CMRLPassengerViewStatus.TicketDetails]
getPassengerViewStatus config req = do
  case config of
    CMRL config' -> CMRLPassengerViewStatus.getPassengerViewStatus config' req
    _ -> error "Unimplemented!"

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> m [CMRLStationList.Station]
getStationList config = do
  case config of
    CMRL config' -> CMRLStationList.getStationList config'
    _ -> error "Unimplemented!"

getTicketStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) => ProviderConfig -> CMRLTicketStatus.TicketStatusReq -> m CMRLTicketStatus.TicketStatusResult
getTicketStatus config req = do
  case config of
    CMRL config' -> CMRLTicketStatus.getTicketStatus config' req
    _ -> error "Unimplemented!"
