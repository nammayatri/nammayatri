{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra where

import qualified Data.Time as Time
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as Domain
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.PgPaymentSettlementReport as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.PgPaymentSettlementReport
import qualified Sequelize as Se

findByReferenceIds ::
  (BeamFlow m r) =>
  [Text] -> -- referenceIds
  m [Domain.PgPaymentSettlementReport]
findByReferenceIds referenceIds =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.referenceId $ Se.In (map Just referenceIds),
          Se.Is Beam.txnStatus $ Se.Eq Domain.SUCCESS
        ]
    ]

findByTxnDateRangeAndStatus ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  UTCTime -> -- startTime
  UTCTime -> -- endTime
  m [Domain.PgPaymentSettlementReport]
findByTxnDateRangeAndStatus merchantId merchantOpCityId startTime endTime = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId,
          Se.Is Beam.txnStatus $ Se.Eq Domain.SUCCESS,
          Se.Is Beam.txnDate $ Se.GreaterThanOrEq (Just startTime),
          Se.Is Beam.txnDate $ Se.LessThanOrEq (Just endTime)
        ]
    ]

findAllByMerchantOpCityIdWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text -> -- subscriptionPurchaseId
  Maybe Text -> -- orderId
  Maybe Text -> -- settlementId
  Maybe Domain.TxnType -> -- transactionType
  Maybe Int ->
  Maybe Int ->
  m [Domain.PgPaymentSettlementReport]
findAllByMerchantOpCityIdWithFilters merchantId merchantOpCityId mbFrom mbTo mbSubscriptionPurchaseId mbOrderId mbSettlementId mbTxnType mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId
        ]
          <> [Se.Is Beam.txnDate $ Se.GreaterThanOrEq (Just fromTime) | Just fromTime <- [mbFrom]]
          <> [Se.Is Beam.txnDate $ Se.LessThanOrEq (Just toTime) | Just toTime <- [mbTo]]
          <> [Se.Is Beam.referenceId $ Se.Eq (Just subscriptionPurchaseId) | Just subscriptionPurchaseId <- [mbSubscriptionPurchaseId]]
          <> [Se.Is Beam.orderId $ Se.Eq orderId | Just orderId <- [mbOrderId]]
          <> [Se.Is Beam.settlementId $ Se.Eq (Just settlementId) | Just settlementId <- [mbSettlementId]]
          <> [Se.Is Beam.txnType $ Se.Eq txnType | Just txnType <- [mbTxnType]]
    ]
    (Se.Desc Beam.createdAt)
    mbLimit
    mbOffset
