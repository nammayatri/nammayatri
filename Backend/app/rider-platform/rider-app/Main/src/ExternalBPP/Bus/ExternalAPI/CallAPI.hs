module ExternalBPP.Bus.ExternalAPI.CallAPI where

import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified ExternalBPP.Bus.ExternalAPI.CUMTA.Order as CUMTAOrder
import qualified ExternalBPP.Bus.ExternalAPI.CUMTA.Status as CUMTAStatus
import qualified ExternalBPP.Bus.ExternalAPI.CUMTA.Verify as CUMTAVerify
import qualified ExternalBPP.Bus.ExternalAPI.EBIX.Order as EBIXOrder
import qualified ExternalBPP.Bus.ExternalAPI.EBIX.Status as EBIXStatus
import ExternalBPP.Bus.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.CacheFlow
import Kernel.Types.Time

getPaymentDetails :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "Unimplemented!"

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ProviderConfig -> Seconds -> FRFSTicketBooking -> m ProviderOrder
createOrder config qrTtl booking = do
  case config of
    EBIX config' -> EBIXOrder.createOrder config' qrTtl booking
    CUMTA config' -> CUMTAOrder.createOrder config' qrTtl booking
    _ -> error "Unimplemented!"

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ProviderConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus config booking = do
  case config of
    EBIX config' -> EBIXStatus.getTicketStatus config' booking
    CUMTA config' -> CUMTAStatus.getTicketStatus config' booking
    _ -> error "Unimplemented!"

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ProviderConfig -> Text -> m TicketPayload
verifyTicket config encryptedQrData = do
  case config of
    CUMTA config' -> CUMTAVerify.verifyTicket config' encryptedQrData
    _ -> error "Unimplemented!"
