module Storage.Queries.PaymentInvoiceExtra where

import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Ride as DRide
import Data.Time (UTCTime(..), Day)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, HighPrecMoney, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PaymentInvoice as BeamPI
import Storage.Queries.PaymentInvoice ()

-- | Find latest payment invoice for a specific date (optimized).
-- Only scans invoices created on the given date using created_at index.
-- Used for initializing the daily invoice sequence counter from database.
findLatestForDate ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Day ->  -- The date to filter by (current day)
  m (Maybe DPI.PaymentInvoice)
findLatestForDate targetDate = do
  -- Get all invoices from the start of this day onwards
  let dayStart = UTCTime targetDate 0  -- Midnight start of day

  findAllWithOptionsKV
    [Se.Is BeamPI.createdAt $ Se.GreaterThanOrEq dayStart]
    (Se.Desc BeamPI.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe


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
