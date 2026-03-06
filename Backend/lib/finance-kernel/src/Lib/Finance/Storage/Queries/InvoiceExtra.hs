module Lib.Finance.Storage.Queries.InvoiceExtra where

import Data.Time (UTCTime (UTCTime), utctDay)
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
-- Filters by merchantOperatingCityId, optional issuedAt range, invoiceType, status, with limit/offset.
findByMerchantOpCityIdAndDateRange ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe DInvoice.InvoiceType ->
  Kernel.Prelude.Maybe DInvoice.InvoiceStatus ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  m [DInvoice.Invoice]
findByMerchantOpCityIdAndDateRange merchantOpCityId mbFrom mbTo mbInvoiceType mbStatus mbLimit mbOffset = do
  let conds =
        [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId]
          <> [Se.Is Beam.issuedAt $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
          <> [Se.Is Beam.issuedAt $ Se.LessThanOrEq to | Just to <- [mbTo]]
          <> [Se.Is Beam.invoiceType $ Se.Eq ty | Just ty <- [mbInvoiceType]]
          <> [Se.Is Beam.status $ Se.Eq st | Just st <- [mbStatus]]
  findAllWithOptionsKV
    (if null conds then [Se.Is Beam.id $ Se.Not $ Se.Eq ""] else [Se.And conds])
    (Se.Desc Beam.issuedAt)
    mbLimit
    mbOffset
