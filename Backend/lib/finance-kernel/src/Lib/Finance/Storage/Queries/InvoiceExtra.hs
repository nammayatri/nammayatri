module Lib.Finance.Storage.Queries.InvoiceExtra where

import Data.Time (UTCTime (UTCTime), utctDay)
import qualified Domain.Types.Invoice as DInvoiceSpec
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.Invoice as DInvoice
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Beam.Invoice as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.Invoice ()
import qualified Sequelize as Se

-- | Find the latest invoice created today (for sequence number fallback).
-- Filters by createdAt >= start of today, ordered by createdAt DESC, limit 1.
findLatestByCreatedAt ::
  (BeamFlow.BeamFlow m r) =>
  UTCTime -> -- current time (used to compute today's start)
  m (Maybe DInvoice.Invoice)
findLatestByCreatedAt now = do
  let todayStart = UTCTime (utctDay now) 0
  findAllWithOptionsKV
    [Se.Is Beam.createdAt $ Se.GreaterThanOrEq todayStart]
    (Se.Desc Beam.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- | Find invoices by merchant op city and optional filters.
-- Filters by merchantOperatingCityId, optional issuedAt range, invoiceType, status, optional issuedToId/supplierId,
-- with limit/offset.
findByMerchantOpCityIdAndDateRange ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe DInvoiceSpec.InvoiceType ->
  Kernel.Prelude.Maybe DInvoice.InvoiceStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- issuedToId (driver/fleet owner)
  Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- supplierId (driver for Ride invoices)
  Kernel.Prelude.Maybe DInvoiceSpec.IssuedToType -> -- issuedToType (DRIVER / RIDER / CUSTOMER / FLEET_OWNER)
  [DInvoice.InvoiceStatus] -> -- statusIn: empty list means no filter; non-empty filters via IN
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  m [DInvoice.Invoice]
findByMerchantOpCityIdAndDateRange merchantOpCityId mbFrom mbTo mbInvoiceType mbStatus mbIssuedToId mbSupplierId mbIssuedToType statusIn mbLimit mbOffset = do
  let conds =
        [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId]
          <> [Se.Is Beam.issuedAt $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
          <> [Se.Is Beam.issuedAt $ Se.LessThanOrEq to | Just to <- [mbTo]]
          <> [Se.Is Beam.invoiceType $ Se.Eq ty | Just ty <- [mbInvoiceType]]
          <> [Se.Is Beam.status $ Se.Eq st | Just st <- [mbStatus]]
          <> [Se.Is Beam.issuedToId $ Se.Eq issuedToId | Just issuedToId <- [mbIssuedToId]]
          <> [Se.Is Beam.supplierId $ Se.Eq (Just supplierId) | Just supplierId <- [mbSupplierId]]
          <> [Se.Is Beam.issuedToType $ Se.Eq issuedToType | Just issuedToType <- [mbIssuedToType]]
          <> [Se.Is Beam.status $ Se.In statusIn | not (null statusIn)]
  findAllWithOptionsKV
    (if null conds then [Se.Is Beam.id $ Se.Not $ Se.Eq ""] else [Se.And conds])
    (Se.Desc Beam.issuedAt)
    mbLimit
    mbOffset

-- | Find invoices issued TO a specific person (driver/fleet) with optional type and date filters.
-- Used for SubscriptionPurchase and RideCancellation where the driver is the issuedTo party.
findByIssuedToAndType ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Prelude.Text -> -- issuedToId (driverId or fleetOwnerId)
  Kernel.Prelude.Maybe DInvoiceSpec.InvoiceType ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  m [DInvoice.Invoice]
findByIssuedToAndType issuedToId mbInvoiceType mbFrom mbTo mbLimit mbOffset = do
  let conds =
        [Se.Is Beam.issuedToId $ Se.Eq issuedToId]
          <> [Se.Is Beam.invoiceType $ Se.Eq ty | Just ty <- [mbInvoiceType]]
          <> [Se.Is Beam.issuedAt $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
          <> [Se.Is Beam.issuedAt $ Se.LessThanOrEq to | Just to <- [mbTo]]
  findAllWithOptionsKV
    [Se.And conds]
    (Se.Desc Beam.issuedAt)
    mbLimit
    mbOffset

-- | Find invoices where a specific person is the supplier, with optional type and date filters.
-- Used for Ride invoices where the driver is the supplier of the service.
findBySupplierAndType ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Prelude.Text -> -- supplierId (driverId)
  Kernel.Prelude.Maybe DInvoiceSpec.InvoiceType ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  m [DInvoice.Invoice]
findBySupplierAndType supplierId mbInvoiceType mbFrom mbTo mbLimit mbOffset = do
  let conds =
        [Se.Is Beam.supplierId $ Se.Eq (Just supplierId)]
          <> [Se.Is Beam.invoiceType $ Se.Eq ty | Just ty <- [mbInvoiceType]]
          <> [Se.Is Beam.issuedAt $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
          <> [Se.Is Beam.issuedAt $ Se.LessThanOrEq to | Just to <- [mbTo]]
  findAllWithOptionsKV
    [Se.And conds]
    (Se.Desc Beam.issuedAt)
    mbLimit
    mbOffset

-- | Find invoices by referenceId with optional invoiceType and status filters.
-- statusIn: empty list means no filter; non-empty filters via IN clause.
findByReferenceIdWithOptions ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe DInvoiceSpec.InvoiceType ->
  [DInvoice.InvoiceStatus] ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  m [DInvoice.Invoice]
findByReferenceIdWithOptions referenceId mbInvoiceType statusIn mbLimit mbOffset = do
  let conds =
        [Se.Is Beam.referenceId $ Se.Eq (Just referenceId)]
          <> [Se.Is Beam.invoiceType $ Se.Eq ty | Just ty <- [mbInvoiceType]]
          <> [Se.Is Beam.status $ Se.In statusIn | not (null statusIn)]
  findAllWithOptionsKV
    [Se.And conds]
    (Se.Desc Beam.issuedAt)
    mbLimit
    mbOffset

-- | Commission invoices in date range (Issued/Draft only). ASC by issuedAt.
-- Type filtering (FLEET_OWNER vs DRIVER) done in Haskell, not SQL — when
-- issuedToId is provided it already pins the recipient. Paginated via
-- (limit, offset) to bound per-query load under concurrent fleet chains.
findCommissionInvoicesInRange ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Prelude.Text -> -- merchantOpCityId
  UTCTime -> -- from
  UTCTime -> -- to
  Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- fleetOwnerId / driverId
  Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- limit
  Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- offset
  m [DInvoice.Invoice]
findCommissionInvoicesInRange merchantOpCityId from to mbFleetOwnerOrDriverId mbLimit mbOffset = do
  let conds =
        [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId]
          <> [Se.Is Beam.invoiceType $ Se.Eq DInvoiceSpec.Commission]
          <> [Se.Is Beam.issuedAt $ Se.GreaterThanOrEq from]
          <> [Se.Is Beam.issuedAt $ Se.LessThanOrEq to]
          <> [Se.Is Beam.status $ Se.In [DInvoice.Issued, DInvoice.Draft]]
          <> [Se.Is Beam.issuedToId $ Se.Eq fid | Just fid <- [mbFleetOwnerOrDriverId]]
  findAllWithOptionsKV
    [Se.And conds]
    (Se.Asc Beam.issuedAt)
    mbLimit
    mbOffset

-- | Highest periodEnd of prior AggregatedCommission rows for this
-- (merchantOpCity, recipient). Drives the runtime idempotency check.
findLatestAggregatedCommissionPeriodEnd ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Prelude.Text -> -- merchantOpCityId
  Kernel.Prelude.Text -> -- issuedToId
  m (Kernel.Prelude.Maybe UTCTime)
findLatestAggregatedCommissionPeriodEnd merchantOpCityId issuedToId = do
  invoices <-
    findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId,
            Se.Is Beam.invoiceType $ Se.Eq DInvoiceSpec.AggregatedCommission,
            Se.Is Beam.issuedToId $ Se.Eq issuedToId,
            Se.Is Beam.status $ Se.In [DInvoice.Issued, DInvoice.Draft]
          ]
      ]
      (Se.Desc Beam.periodEnd)
      (Just 1)
      Nothing
  pure $ listToMaybe invoices >>= (.periodEnd)

-- | DB fallback for the AggregatedCommission Redis sequence ("CMB" counter).
-- Scoped to AggregatedCommission rows so the isolated series is recovered
-- correctly on Redis miss.
findLatestAggregatedCommissionByCreatedAt ::
  (BeamFlow.BeamFlow m r) =>
  UTCTime ->
  m (Kernel.Prelude.Maybe DInvoice.Invoice)
findLatestAggregatedCommissionByCreatedAt now = do
  let todayStart = UTCTime (utctDay now) 0
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.invoiceType $ Se.Eq DInvoiceSpec.AggregatedCommission,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq todayStart
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe
