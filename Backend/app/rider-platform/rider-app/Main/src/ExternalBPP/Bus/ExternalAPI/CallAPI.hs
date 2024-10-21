module ExternalBPP.Bus.ExternalAPI.CallAPI where

import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified ExternalBPP.Bus.ExternalAPI.CUMTA.Order as CUMTAOrder
import qualified ExternalBPP.Bus.ExternalAPI.CUMTA.Verification as CUMTAVerification
import qualified ExternalBPP.Bus.ExternalAPI.EBIX.Order as EBIXOrder
import qualified ExternalBPP.Bus.ExternalAPI.EBIX.Status as EBIXStatus
import qualified ExternalBPP.Bus.ExternalAPI.EBIX.Verification as EBIXVerification
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.CacheFlow
import Kernel.Types.Time

getOrderId :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ProviderConfig -> FRFSTicketBooking -> m (Text, Integer)
getOrderId config booking = do
  case config of
    EBIX config' -> EBIXOrder.getOrderId config' booking
    CUMTA config' -> CUMTAOrder.getOrderId config' booking
    _ -> error "Unimplemented!"

getPaymentDetails :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "Unimplemented!"

getTicketStatus :: (CoreMetrics m, MonadFlow m, EncFlow m r, CacheFlow m r) => ProviderConfig -> FRFSTicketBooking -> Text -> m Text
getTicketStatus config booking txnUUID = do
  case config of
    EBIX config' -> EBIXStatus.getTicketStatus config' booking txnUUID
    CUMTA _ -> return "UNCLAIMED"
    _ -> error "Unimplemented!"

generateQRByProvider :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ProviderConfig -> Text -> Integer -> Seconds -> FRFSTicketBooking -> m (Text, Text, UTCTime, Text)
generateQRByProvider config txnUUID ticketNum qrTtl booking = do
  case config of
    EBIX config' -> EBIXVerification.generateQRByProvider config' txnUUID ticketNum qrTtl booking
    CUMTA config' -> CUMTAVerification.generateQR config' txnUUID ticketNum qrTtl booking
    _ -> error "Unimplemented!"
