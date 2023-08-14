{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Invoice where

import Domain.Types.DriverFee (DriverFee)
import Domain.Types.Invoice as Domain
import qualified EulerHS.Language as L
import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findOneWithKV, updateWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Types.Time
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

findByDriverFeeIdAndActiveStatus :: (L.MonadFlow m, Log m) => Id DriverFee -> m (Maybe Domain.Invoice)
findByDriverFeeIdAndActiveStatus (Id driverFeeId) = findOneWithKV [Se.And [Se.Is BeamI.driverFeeId $ Se.Eq driverFeeId, Se.Is BeamI.invoiceStatus $ Se.Eq Domain.ACTIVE_INVOICE]]

updateInvoiceStatusByInvoiceId :: (L.MonadFlow m, Log m, MonadTime m) => Id Domain.Invoice -> Domain.InvoiceStatus -> m ()
updateInvoiceStatusByInvoiceId invoiceId invoiceStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.invoiceStatus invoiceStatus,
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId invoiceId)]

instance FromTType' BeamI.Invoice Domain.Invoice where
  fromTType' BeamI.InvoiceT {..} = do
    pure $
      Just
        Invoice
          { id = Id id,
            invoiceShortId = invoiceShortId,
            driverFeeId = Id driverFeeId,
            invoiceStatus = invoiceStatus,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamI.Invoice Domain.Invoice where
  toTType' Invoice {..} = do
    BeamI.InvoiceT
      { BeamI.id = id.getId,
        BeamI.invoiceShortId = invoiceShortId,
        BeamI.driverFeeId = getId driverFeeId,
        BeamI.invoiceStatus = invoiceStatus,
        BeamI.createdAt = createdAt,
        BeamI.updatedAt = updatedAt
      }
