module ExternalBPP.Bus.ExternalAPI.CUMTA.Verify where

import Domain.Types.IntegratedBPPConfig
import ExternalBPP.Bus.ExternalAPI.CUMTA.Utils
import ExternalBPP.Bus.ExternalAPI.Types
import Kernel.Prelude
import Kernel.Utils.Common

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CUMTAConfig -> Text -> m TicketPayload
verifyTicket config encryptedQrData = decodeQR config encryptedQrData
