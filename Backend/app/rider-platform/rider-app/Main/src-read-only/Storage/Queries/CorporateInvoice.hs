{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateInvoice (module Storage.Queries.CorporateInvoice, module ReExport) where

import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporateInvoice
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateInvoice as Beam
import Storage.Queries.CorporateInvoiceExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateInvoice ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateInvoice.CorporateInvoice -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateInvoice.CorporateInvoice] -> m ())
createMany = traverse_ create

findByCorporateEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m [Domain.Types.CorporateInvoice.CorporateInvoice])
findByCorporateEntityId corporateEntityId = do findAllWithKV [Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId)]

findByInvoiceNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> m (Maybe Domain.Types.CorporateInvoice.CorporateInvoice))
findByInvoiceNumber invoiceNumber = do findOneWithKV [Se.Is Beam.invoiceNumber $ Se.Eq invoiceNumber]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.CorporateInvoice.CorporateInvoiceStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateInvoice.CorporateInvoice -> m ())
updateStatus status updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status (Kernel.Prelude.show status),
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateInvoice.CorporateInvoice -> m (Maybe Domain.Types.CorporateInvoice.CorporateInvoice))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
