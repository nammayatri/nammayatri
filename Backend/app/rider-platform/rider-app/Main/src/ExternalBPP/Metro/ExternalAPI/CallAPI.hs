module ExternalBPP.Metro.ExternalAPI.CallAPI where

import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.CacheFlow
import Kernel.Types.Time

getOrderId :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ProviderConfig -> FRFSTicketBooking -> m Text
getOrderId config _booking = do
  case config of
    _ -> error "Unimplemented!"

getPaymentDetails :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "Unimplemented!"

getTicketStatus :: (CoreMetrics m, MonadFlow m, EncFlow m r, CacheFlow m r) => ProviderConfig -> FRFSTicketBooking -> Text -> m Text
getTicketStatus config _booking _txnUUID = do
  case config of
    _ -> error "Unimplemented!"

generateQRByProvider :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ProviderConfig -> Text -> Seconds -> FRFSTicketBooking -> m [(Text, Text, UTCTime, Text)]
generateQRByProvider config _txnUUID _qrTtl _booking = do
  case config of
    _ -> error "Unimplemented!"
