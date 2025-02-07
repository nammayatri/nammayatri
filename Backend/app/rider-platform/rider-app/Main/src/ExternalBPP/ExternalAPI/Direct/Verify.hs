module ExternalBPP.ExternalAPI.Direct.Verify where

import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Direct.Utils
import ExternalBPP.ExternalAPI.Types
import Kernel.Prelude
import Kernel.Utils.Common

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DIRECTConfig -> Text -> m TicketPayload
verifyTicket config encryptedQrData = decodeQR config encryptedQrData
