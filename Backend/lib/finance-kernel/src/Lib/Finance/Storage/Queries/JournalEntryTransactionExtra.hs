{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.JournalEntryTransactionExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.JournalEntryTransaction as Domain (JournalEntryTransaction)
import Lib.Finance.Domain.Types.SapJournalEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.JournalEntryTransaction as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.JournalEntryTransaction ()
import qualified Sequelize as Se

findByMerchantIdWithFilters ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  TransactionType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.JournalEntryTransaction]
findByMerchantIdWithFilters merchantId merchantOperatingCityId mbFrom mbTo transactionType mbSubscriptionId mbStatus mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.transactionType $ Se.Eq transactionType
        ]
          <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromDate | Just fromDate <- [mbFrom]]
          <> [Se.Is Beam.createdAt $ Se.LessThanOrEq toDate | Just toDate <- [mbTo]]
          <> [Se.Is Beam.subscriptionId $ Se.Eq (Just subId) | Just subId <- [mbSubscriptionId]]
          <> [Se.Is Beam.status $ Se.Eq st | Just st <- [mbStatus]]
    ]
    (Se.Desc Beam.createdAt)
    (Just $ min 100 $ fromMaybe 20 mbLimit)
    mbOffset
