{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Invoice where

import Domain.Types.Invoice as Domain
import qualified EulerHS.Language as L
import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import qualified Sequelize as Se
import Storage.Beam.Invoice as BeamI hiding (Id)

-- createMany :: [Invoice] -> SqlDB ()
-- createMany = Esq.createMany

create :: (L.MonadFlow m, Log m) => Domain.Invoice -> m ()
create = createWithKV

createMany :: (L.MonadFlow m, Log m) => [Domain.Invoice] -> m ()
createMany = traverse_ create

-- findAllByInvoiceId ::
--   Transactionable m =>
--   Id Invoice ->
--   m [Invoice]
-- findAllByInvoiceId invoiceId = do
--   Esq.findAll $ do
--     invoice <- from $ table @InvoiceT
--     where_ $
--       invoice ^. InvoiceId ==. val (invoiceId.getId)
--     return invoice

findAllByInvoiceId :: (L.MonadFlow m, Log m) => Id Domain.Invoice -> m [Domain.Invoice]
findAllByInvoiceId (Id invoiceId) = findAllWithKV [Se.Is BeamI.id $ Se.Eq invoiceId]

instance FromTType' BeamI.Invoice Domain.Invoice where
  fromTType' BeamI.InvoiceT {..} = do
    pure $
      Just
        Invoice
          { id = id,
            invoiceShortId = invoiceShortId,
            driverFeeId = Id driverFeeId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamI.Invoice Domain.Invoice where
  toTType' Invoice {..} = do
    BeamI.InvoiceT
      { BeamI.id = id,
        BeamI.invoiceShortId = invoiceShortId,
        BeamI.driverFeeId = getId driverFeeId,
        BeamI.createdAt = createdAt,
        BeamI.updatedAt = updatedAt
      }
