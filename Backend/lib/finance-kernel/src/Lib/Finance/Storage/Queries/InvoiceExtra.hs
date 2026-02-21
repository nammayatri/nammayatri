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
