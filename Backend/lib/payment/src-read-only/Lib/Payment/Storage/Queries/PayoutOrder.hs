{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutOrder (module Lib.Payment.Storage.Queries.PayoutOrder, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutOrder as Beam
import Lib.Payment.Storage.Queries.PayoutOrderExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder] -> m ())
createMany = traverse_ create

findAllByCustomerId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder]))
findAllByCustomerId customerId = do findAllWithKV [Se.Is Beam.customerId $ Se.Eq customerId]

findAllWithStatus :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus -> m ([Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder]))
findAllWithStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> m (Maybe Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByOrderId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder))
findByOrderId orderId = do findOneWithKV [Se.Is Beam.orderId $ Se.Eq orderId]

updatePayoutOrderStatus :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus -> Kernel.Prelude.Text -> m ())
updatePayoutOrderStatus status orderId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.orderId $ Se.Eq orderId]

updatePayoutOrderTxnRespInfo ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updatePayoutOrderTxnRespInfo responseCode responseMessage orderId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.responseCode responseCode, Se.Set Beam.responseMessage responseMessage, Se.Set Beam.updatedAt _now] [Se.Is Beam.orderId $ Se.Eq orderId]

updateRetriedOrderId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateRetriedOrderId retriedOrderId orderId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.retriedOrderId retriedOrderId, Se.Set Beam.updatedAt _now] [Se.Is Beam.orderId $ Se.Eq orderId]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder))
findByPrimaryKey id orderId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.orderId $ Se.Eq orderId]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountDetailsType accountDetailsType,
      Se.Set Beam.currency (((Kernel.Prelude.Just . (.currency))) amount),
      Se.Set Beam.price ((.amount) amount),
      Se.Set Beam.city city,
      Se.Set Beam.customerEmailEncrypted (((customerEmail & unEncrypted . encrypted))),
      Se.Set Beam.customerEmailHash ((customerEmail & hash)),
      Se.Set Beam.customerId customerId,
      Se.Set Beam.entityIds entityIds,
      Se.Set Beam.entityName entityName,
      Se.Set Beam.lastStatusCheckedAt lastStatusCheckedAt,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.mobileNoEncrypted (((mobileNo & unEncrypted . encrypted))),
      Se.Set Beam.mobileNoHash ((mobileNo & hash)),
      Se.Set Beam.responseCode responseCode,
      Se.Set Beam.responseMessage responseMessage,
      Se.Set Beam.retriedOrderId retriedOrderId,
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId <$> shortId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vpa vpa
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.orderId $ Se.Eq orderId]]
