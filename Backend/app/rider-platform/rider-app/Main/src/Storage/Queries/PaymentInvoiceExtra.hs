module Storage.Queries.PaymentInvoiceExtra where

import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, HighPrecMoney, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PaymentInvoice as BeamPI
import Storage.Queries.PaymentInvoice ()

-- | Find latest payment invoice globally (for global sequence).
-- Ordered by createdAt DESC with LIMIT 1.
-- Uses KV layer which updates immediately.
findLatestGlobal ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  m (Maybe DPI.PaymentInvoice)
findLatestGlobal =
  findAllWithOptionsKV [] (Se.Desc BeamPI.createdAt) (Just 1) Nothing <&> listToMaybe

-- | Find payment invoice by rideId, invoiceType, and paymentPurpose.
-- Used to lookup specific invoice to handle cases with multiple purposes (RIDE, TIP, CANCELLATION_FEE).
findByRideIdAndTypeAndPurpose ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRide.Ride ->
  DPI.InvoiceType ->
  DPI.PaymentPurpose ->
  m (Maybe DPI.PaymentInvoice)
findByRideIdAndTypeAndPurpose rideId invoiceType purpose =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamPI.rideId $ Se.Eq (getId rideId),
          Se.Is BeamPI.invoiceType $ Se.Eq invoiceType,
          Se.Is BeamPI.paymentPurpose $ Se.Eq purpose
        ]
    ]

-- | Update payment status for refund invoice by rideId, invoiceType, and paymentPurpose.
-- Used to update specific refund invoice status when multiple purposes exist.
updatePaymentStatusByRideIdAndTypeAndPurpose ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRide.Ride ->
  DPI.InvoiceType ->
  DPI.PaymentPurpose ->
  DPI.InvoicePaymentStatus ->
  m ()
updatePaymentStatusByRideIdAndTypeAndPurpose rideId invoiceType purpose newStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamPI.paymentStatus newStatus,
      Se.Set BeamPI.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamPI.rideId $ Se.Eq (getId rideId),
          Se.Is BeamPI.invoiceType $ Se.Eq invoiceType,
          Se.Is BeamPI.paymentPurpose $ Se.Eq purpose
        ]
    ]

-- | Update invoice amount by invoice ID.
-- Used when fare changes after invoice creation (e.g., toll recalculation).
updateAmount ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DPI.PaymentInvoice ->
  HighPrecMoney ->
  m ()
updateAmount invoiceId newAmount = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamPI.amount newAmount,
      Se.Set BeamPI.updatedAt now
    ]
    [Se.Is BeamPI.id $ Se.Eq (getId invoiceId)]

-- | Find failed payment invoices for a ride (excluding settled ones and DEBT_SETTLEMENT purpose).
-- Used by Get Dues API to calculate pending dues.
-- Note: Filtering for DEBT_SETTLEMENT and TIP purposes happens in application code for flexibility
findFailedUnsettledByRideId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRide.Ride ->
  m [DPI.PaymentInvoice]
findFailedUnsettledByRideId rideId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamPI.rideId $ Se.Eq (getId rideId),
          Se.Is BeamPI.invoiceType $ Se.Eq DPI.PAYMENT,
          Se.Is BeamPI.paymentStatus $ Se.Eq DPI.FAILED,
          Se.Is BeamPI.settledByInvoiceId $ Se.Eq Nothing
        ]
    ]

-- | Update settled_by_invoice_id for a single invoice.
-- Used when a DEBT_SETTLEMENT invoice successfully settles parent invoices.
updateSettledBy ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DPI.PaymentInvoice ->
  Maybe (Id DPI.PaymentInvoice) ->
  m ()
updateSettledBy invoiceId mbSettledById = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamPI.settledByInvoiceId (getId <$> mbSettledById),
      Se.Set BeamPI.updatedAt now
    ]
    [Se.Is BeamPI.id $ Se.Eq (getId invoiceId)]

-- | Update settled_by_invoice_id for multiple invoices (batch operation).
-- Used when a DEBT_SETTLEMENT invoice successfully settles multiple parent invoices.
updateSettledByForInvoices ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id DPI.PaymentInvoice] ->
  Id DPI.PaymentInvoice ->
  m ()
updateSettledByForInvoices invoiceIds settledById = do
  forM_ invoiceIds $ \invoiceId ->
    updateSettledBy invoiceId (Just settledById)
