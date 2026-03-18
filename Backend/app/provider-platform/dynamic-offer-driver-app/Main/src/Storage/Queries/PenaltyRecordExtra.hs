module Storage.Queries.PenaltyRecordExtra where

import qualified Domain.Types.Merchant
import qualified Domain.Types.PenaltyRecord
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.PenaltyRecord as Beam

-- Extra code goes here --

findAllByMerchantIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Maybe Domain.Types.PenaltyRecord.PenaltyStatus ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.PenaltyRecord.PenaltyRecord]
findAllByMerchantIdWithLimitOffset merchantId mbStatus mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

findAllByDriverIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Domain.Types.PenaltyRecord.PenaltyStatus ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.PenaltyRecord.PenaltyRecord]
findAllByDriverIdWithLimitOffset driverId mbStatus mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

-- | Count penalty records by merchant and status for analytics.
-- Returns list of (status, count) pairs using DB-level counting per status.
countByMerchantIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Domain.Types.PenaltyRecord.PenaltyStatus ->
  m Int
countByMerchantIdAndStatus merchantId status = do
  records <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
            Se.Is Beam.status $ Se.Eq status
          ]
      ]
  pure $ length (records :: [Domain.Types.PenaltyRecord.PenaltyRecord])

-- | [m9 fix] Update dispute resolution with resolver note.
-- Stores the resolverNote in disputeReason (appended after original reason if present).
updateDisputeResolutionWithNoteById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Domain.Types.PenaltyRecord.PenaltyStatus ->
  Maybe Kernel.Prelude.Text -> -- disputeResolvedBy
  Maybe Kernel.Prelude.UTCTime -> -- disputeResolvedAt
  Maybe Kernel.Prelude.Text -> -- resolverNote (stored in disputeEvidence for audit)
  Kernel.Prelude.UTCTime -> -- updatedAt
  Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord ->
  m ()
updateDisputeResolutionWithNoteById status disputeResolvedBy disputeResolvedAt resolverNote updatedAt id = do
  updateWithKV
    ( [ Se.Set Beam.status status,
        Se.Set Beam.disputeResolvedBy disputeResolvedBy,
        Se.Set Beam.disputeResolvedAt disputeResolvedAt,
        Se.Set Beam.updatedAt updatedAt
      ]
        <> [Se.Set Beam.disputeEvidence resolverNote | isJust resolverNote]
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

-- | Sum penalty amounts by merchant for analytics
sumAmountByMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  m Kernel.Types.Common.HighPrecMoney
sumAmountByMerchantId merchantId = do
  records <-
    findAllWithKV
      [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]
  pure $ sum $ map (.amount) (records :: [Domain.Types.PenaltyRecord.PenaltyRecord])
