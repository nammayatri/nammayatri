module Storage.Queries.PaymentInvoiceExtra where

import qualified Database.Beam as B
import qualified Domain.Types.PaymentInvoice as DPI
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.PaymentInvoice as BeamPI
import Storage.Queries.PaymentInvoice ()

-- | Find latest payment invoice globally (for global sequence).
-- Ordered by createdAt DESC with LIMIT 1.
findLatestGlobal ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  m (Maybe DPI.PaymentInvoice)
findLatestGlobal = do
  dbConf <- getReplicaBeamConfig
  result <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ 1 $
            B.orderBy_ (\pi' -> B.desc_ (BeamPI.createdAt pi')) $
              B.all_ (BeamCommon.paymentInvoice BeamCommon.atlasDB)
  case result of
    Right rows -> case rows of
      [] -> pure Nothing
      (piRow : _) -> fromTType' piRow
    Left _ -> pure Nothing
