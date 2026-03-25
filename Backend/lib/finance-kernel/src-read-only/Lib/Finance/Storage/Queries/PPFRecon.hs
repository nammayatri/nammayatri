{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PPFRecon where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PPFRecon
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.PPFRecon as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PPFRecon.PPFRecon -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.PPFRecon.PPFRecon] -> m ())
createMany = traverse_ create

findByNetworkOrderId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.PPFRecon.PPFRecon))
findByNetworkOrderId networkOrderId = do findOneWithKV [Se.And [Se.Is Beam.networkOrderId $ Se.Eq networkOrderId]]

findByTransactionId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.PPFRecon.PPFRecon]))
findByTransactionId transactionId = do findAllWithKV [Se.And [Se.Is Beam.transactionId $ Se.Eq transactionId]]

findPendingReconciliation ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.PPFRecon.PPFSettlementStatus -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.PPFRecon.PPFRecon]))
findPendingReconciliation settlementStatus merchantId = do findAllWithKV [Se.And [Se.Is Beam.settlementStatus $ Se.Eq settlementStatus, Se.Is Beam.merchantId $ Se.Eq merchantId]]

updateSettlementStatusById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.PPFRecon.PPFSettlementStatus -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.PPFRecon.PPFRecon -> m ())
updateSettlementStatusById settlementStatus id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.settlementStatus settlementStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PPFRecon.PPFRecon -> m (Maybe Lib.Finance.Domain.Types.PPFRecon.PPFRecon))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PPFRecon.PPFRecon -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.PPFRecon.PPFRecon {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.buyerAppCommissionExpected ((.expected) buyerAppCommission),
      Se.Set Beam.buyerAppCommissionSettled ((.settled) buyerAppCommission),
      Se.Set Beam.collectorSubscriberId collectorSubscriberId,
      Se.Set Beam.currency currency,
      Se.Set Beam.domain domain,
      Se.Set Beam.gstAmountExpected ((>>= (.expected)) gstAmount),
      Se.Set Beam.gstAmountSettled (Kernel.Prelude.fmap (.settled) gstAmount),
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.networkFeeExpected ((>>= (.expected)) networkFee),
      Se.Set Beam.networkFeeSettled (Kernel.Prelude.fmap (.settled) networkFee),
      Se.Set Beam.networkOrderId networkOrderId,
      Se.Set Beam.orderAmountExpected ((.expected) orderAmount),
      Se.Set Beam.orderAmountSettled ((.settled) orderAmount),
      Se.Set Beam.receiverSubscriberId receiverSubscriberId,
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.settlementId settlementId,
      Se.Set Beam.settlementStatus settlementStatus,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.utr utr
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

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
