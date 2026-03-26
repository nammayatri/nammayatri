{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra where

import qualified Data.Time as Time
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney (..))
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
  Maybe Text -> -- pgApprovalCode
  Maybe Text -> -- utr
  Maybe UTCTime -> -- settlementFrom
  Maybe UTCTime -> -- settlementTo
  Maybe Text -> -- pgName (paymentGateway)
  Maybe HighPrecMoney -> -- settlementAmountMin
  Maybe HighPrecMoney -> -- settlementAmountMax
  Maybe HighPrecMoney -> -- txnAmountMin
  Maybe HighPrecMoney -> -- txnAmountMax
  Maybe Int ->
  Maybe Int ->
  m [Domain.PgPaymentSettlementReport]
findAllByMerchantOpCityIdWithFilters merchantId merchantOpCityId mbFrom mbTo mbSubscriptionPurchaseId mbOrderId mbSettlementId mbTxnType mbPgApprovalCode mbUtr mbSettlementFrom mbSettlementTo mbPgName mbSettlementAmountMin mbSettlementAmountMax mbTxnAmountMin mbTxnAmountMax mbLimit mbOffset =
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
          <> [Se.Is Beam.pgApprovalCode $ Se.Eq (Just code) | Just code <- [mbPgApprovalCode]]
          <> [Se.Is Beam.utr $ Se.Eq (Just u) | Just u <- [mbUtr]]
          <> [Se.Is Beam.settlementDate $ Se.GreaterThanOrEq (Just t) | Just t <- [mbSettlementFrom]]
          <> [Se.Is Beam.settlementDate $ Se.LessThanOrEq (Just t) | Just t <- [mbSettlementTo]]
          <> [Se.Is Beam.paymentGateway $ Se.Eq (Just name) | Just name <- [mbPgName]]
          <> [Se.Is Beam.settlementAmount $ Se.GreaterThanOrEq minAmt | Just minAmt <- [mbSettlementAmountMin]]
          <> [Se.Is Beam.settlementAmount $ Se.LessThanOrEq maxAmt | Just maxAmt <- [mbSettlementAmountMax]]
          <> [Se.Is Beam.txnAmount $ Se.GreaterThanOrEq minAmt | Just minAmt <- [mbTxnAmountMin]]
          <> [Se.Is Beam.txnAmount $ Se.LessThanOrEq maxAmt | Just maxAmt <- [mbTxnAmountMax]]
    ]
    (Se.Desc Beam.createdAt)
    mbLimit
    mbOffset

findByOrderIdAndTxnType ::
  (BeamFlow m r) =>
  Text ->
  Domain.TxnType ->
  m [Domain.PgPaymentSettlementReport]
findByOrderIdAndTxnType orderId txnType =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.orderId $ Se.Eq orderId,
          Se.Is Beam.txnType $ Se.Eq txnType
        ]
    ]

findByMerchantIdAndDateRange ::
  (BeamFlow m r) =>
  Text ->
  UTCTime ->
  UTCTime ->
  m [Domain.PgPaymentSettlementReport]
findByMerchantIdAndDateRange merchantId fromDate toDate =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq toDate
        ]
    ]

findBySettlementId ::
  (BeamFlow m r) =>
  Text ->
  m [Domain.PgPaymentSettlementReport]
findBySettlementId settlementId =
  findAllWithKV [Se.Is Beam.settlementId $ Se.Eq (Just settlementId)]
