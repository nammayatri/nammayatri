{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Invoice (module Storage.Queries.Invoice, module ReExport) where

import qualified Domain.Types.DriverFee
import qualified Domain.Types.Invoice
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Invoice as Beam
import Storage.Queries.InvoiceExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Invoice.Invoice -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Invoice.Invoice] -> m ())
createMany = traverse_ create

findActiveManualInvoiceByFeeId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> Domain.Types.Invoice.InvoicePaymentMode -> Domain.Types.Invoice.InvoiceStatus -> m [Domain.Types.Invoice.Invoice])
findActiveManualInvoiceByFeeId driverFeeId paymentMode invoiceStatus = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId),
          Se.Is Beam.paymentMode $ Se.Eq paymentMode,
          Se.Is Beam.invoiceStatus $ Se.Eq invoiceStatus
        ]
    ]

findAllByInvoiceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> m [Domain.Types.Invoice.Invoice])
findAllByInvoiceId id = do findAllWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByInvoiceShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.Invoice.Invoice])
findAllByInvoiceShortId invoiceShortId = do findAllWithKV [Se.Is Beam.invoiceShortId $ Se.Eq invoiceShortId]

findByDriverFeePaymentModeAndInvoiceStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee -> Domain.Types.Invoice.InvoicePaymentMode -> Domain.Types.Invoice.InvoiceStatus -> m [Domain.Types.Invoice.Invoice])
findByDriverFeePaymentModeAndInvoiceStatus driverFeeId paymentMode invoiceStatus = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId),
          Se.Is Beam.paymentMode $ Se.Eq paymentMode,
          Se.Is Beam.invoiceStatus $ Se.Eq invoiceStatus
        ]
    ]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> m [Domain.Types.Invoice.Invoice])
findById id = do findAllWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByIdWithPaymenModeAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Domain.Types.Invoice.InvoicePaymentMode -> Domain.Types.Invoice.InvoiceStatus -> m (Maybe Domain.Types.Invoice.Invoice))
findByIdWithPaymenModeAndStatus id paymentMode invoiceStatus = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.paymentMode $ Se.Eq paymentMode,
          Se.Is Beam.invoiceStatus $ Se.Eq invoiceStatus
        ]
    ]

updateBankErrorsByInvoiceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> m ())
updateBankErrorsByInvoiceId bankErrorMessage bankErrorCode bankErrorUpdatedAt id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bankErrorMessage bankErrorMessage,
      Se.Set Beam.bankErrorCode bankErrorCode,
      Se.Set Beam.bankErrorUpdatedAt bankErrorUpdatedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> m (Maybe Domain.Types.Invoice.Invoice))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Invoice.Invoice -> m ())
updateByPrimaryKey (Domain.Types.Invoice.Invoice {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bankErrorCode bankErrorCode,
      Se.Set Beam.bankErrorMessage bankErrorMessage,
      Se.Set Beam.bankErrorUpdatedAt bankErrorUpdatedAt,
      Se.Set Beam.driverFeeId (Kernel.Types.Id.getId driverFeeId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.invoiceShortId invoiceShortId,
      Se.Set Beam.invoiceStatus invoiceStatus,
      Se.Set Beam.lastStatusCheckedAt lastStatusCheckedAt,
      Se.Set Beam.maxMandateAmount maxMandateAmount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Just (Kernel.Types.Id.getId merchantOperatingCityId)),
      Se.Set Beam.paymentMode paymentMode,
      Se.Set Beam.serviceName (Just serviceName),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
