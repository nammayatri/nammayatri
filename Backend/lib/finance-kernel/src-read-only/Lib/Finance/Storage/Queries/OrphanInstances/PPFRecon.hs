{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.PPFRecon where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PPFRecon
import qualified Lib.Finance.Storage.Beam.PPFRecon as Beam

instance FromTType' Beam.PPFRecon Lib.Finance.Domain.Types.PPFRecon.PPFRecon where
  fromTType' (Beam.PPFReconT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.PPFRecon.PPFRecon
          { buyerAppCommission = Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount buyerAppCommissionExpected buyerAppCommissionSettled,
            collectorSubscriberId = collectorSubscriberId,
            createdAt = createdAt,
            currency = currency,
            domain = domain,
            gstAmount = Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount <$> Kernel.Prelude.pure gstAmountExpected <*> gstAmountSettled,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            networkFee = Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount <$> Kernel.Prelude.pure networkFeeExpected <*> networkFeeSettled,
            networkOrderId = networkOrderId,
            orderAmount = Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount orderAmountExpected orderAmountSettled,
            receiverSubscriberId = receiverSubscriberId,
            settlementDate = settlementDate,
            settlementId = settlementId,
            settlementStatus = settlementStatus,
            transactionId = transactionId,
            updatedAt = updatedAt,
            utr = utr
          }

instance ToTType' Beam.PPFRecon Lib.Finance.Domain.Types.PPFRecon.PPFRecon where
  toTType' (Lib.Finance.Domain.Types.PPFRecon.PPFRecon {..}) = do
    Beam.PPFReconT
      { Beam.buyerAppCommissionExpected = (.expected) buyerAppCommission,
        Beam.buyerAppCommissionSettled = (.settled) buyerAppCommission,
        Beam.collectorSubscriberId = collectorSubscriberId,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.domain = domain,
        Beam.gstAmountExpected = (>>= (.expected)) gstAmount,
        Beam.gstAmountSettled = Kernel.Prelude.fmap (.settled) gstAmount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.networkFeeExpected = (>>= (.expected)) networkFee,
        Beam.networkFeeSettled = Kernel.Prelude.fmap (.settled) networkFee,
        Beam.networkOrderId = networkOrderId,
        Beam.orderAmountExpected = (.expected) orderAmount,
        Beam.orderAmountSettled = (.settled) orderAmount,
        Beam.receiverSubscriberId = receiverSubscriberId,
        Beam.settlementDate = settlementDate,
        Beam.settlementId = settlementId,
        Beam.settlementStatus = settlementStatus,
        Beam.transactionId = transactionId,
        Beam.updatedAt = updatedAt,
        Beam.utr = utr
      }
