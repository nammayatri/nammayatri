{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.SapJournalEntryExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.SapJournalEntry as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.SapJournalEntry as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.SapJournalEntry
import qualified Sequelize as Se

findLatestBatchId ::
  (BeamFlow m r) =>
  m (Maybe Text)
findLatestBatchId = do
  mbEntry <- findAllWithOptionsKV [] (Se.Desc Beam.createdAt) (Just 1) Nothing
  pure $ case mbEntry of
    [entry] -> Just (entry :: Domain.SapJournalEntry).batchId
    _ -> Nothing

findByMerchantIdWithFilters ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Domain.TransactionType ->
  Maybe Domain.JournalEntryStatus ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.SapJournalEntry]
findByMerchantIdWithFilters merchantId merchantOperatingCityId mbDateFrom mbDateTo mbTransactionType mbStatus mbBatchId mbBelnr mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
        ]
          <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromDate | Just fromDate <- [mbDateFrom]]
          <> [Se.Is Beam.createdAt $ Se.LessThanOrEq toDate | Just toDate <- [mbDateTo]]
          <> [Se.Is Beam.transactionType $ Se.Eq txnType | Just txnType <- [mbTransactionType]]
          <> [Se.Is Beam.status $ Se.Eq st | Just st <- [mbStatus]]
          <> [Se.Is Beam.batchId $ Se.Eq bid | Just bid <- [mbBatchId]]
          <> [Se.Is Beam.belnr $ Se.Eq (Just belnr) | Just belnr <- [mbBelnr]]
    ]
    (Se.Desc Beam.createdAt)
    (Just $ min 100 $ fromMaybe 20 mbLimit)
    mbOffset
