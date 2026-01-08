{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RefundRequest (module Storage.Queries.RefundRequest, module ReExport) where

import qualified Domain.Types.RefundRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.Refunds
import qualified Sequelize as Se
import qualified Storage.Beam.RefundRequest as Beam
import Storage.Queries.RefundRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RefundRequest.RefundRequest -> m ())
create = createWithKV

findByOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe Domain.Types.RefundRequest.RefundRequest))
findByOrderId orderId = do findOneWithKV [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getId orderId)]

findByRefundsId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds) -> m (Maybe Domain.Types.RefundRequest.RefundRequest))
findByRefundsId refundsId = do findOneWithKV [Se.Is Beam.refundsId $ Se.Eq (Kernel.Types.Id.getId <$> refundsId)]

updateRefundDetails ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> m ())
updateRefundDetails status responseDescription refundsAmount refundsTries id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.responseDescription responseDescription,
      Se.Set Beam.refundsAmount refundsAmount,
      Se.Set Beam.refundsTries refundsTries,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRefundIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds) -> Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> m ())
updateRefundIdAndStatus refundsId status id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.refundsId (Kernel.Types.Id.getId <$> refundsId), Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRefundStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> m ())
updateRefundStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
