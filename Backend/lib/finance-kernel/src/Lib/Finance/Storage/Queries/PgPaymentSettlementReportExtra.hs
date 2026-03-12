{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgPaymentSettlementReportExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Lib.Finance.Storage.Queries.OrphanInstances.PgPaymentSettlementReport

-- Extra code goes here --

findAllByMerchantOpCityIdWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text -> -- subscriptionPurchaseId
  Maybe Text -> -- orderId
  Maybe Text -> -- settlementId
  Maybe Int ->
  Maybe Int ->
  m [Domain.PgPaymentSettlementReport]
findAllByMerchantOpCityIdWithFilters merchantId merchantOpCityId mbFrom mbTo mbSubscriptionPurchaseId mbOrderId mbSettlementId mbLimit mbOffset =
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
    ]
    (Se.Desc Beam.createdAt)
    mbLimit
    mbOffset
