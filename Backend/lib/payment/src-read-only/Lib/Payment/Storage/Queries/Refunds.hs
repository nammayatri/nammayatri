{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.Refunds (module Lib.Payment.Storage.Queries.Refunds, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.Refunds
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.Refunds as Beam
import Lib.Payment.Storage.Queries.RefundsExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
create = createWithKV

findAllByOrderId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m [Lib.Payment.Domain.Types.Refunds.Refunds])
findAllByOrderId orderId = do findAllWithKV [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getShortId orderId)]

findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m (Maybe Lib.Payment.Domain.Types.Refunds.Refunds))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByShortId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.Refunds.Refunds -> m (Maybe Lib.Payment.Domain.Types.Refunds.Refunds))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

updateIsApiCallSuccess :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
updateIsApiCallSuccess isApiCallSuccess id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.isApiCallSuccess isApiCallSuccess, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRefundsEntryByResponse ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Payment.Interface.RefundStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
updateRefundsEntryByResponse initiatedBy idAssignedByServiceProvider errorMessage errorCode status arn id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.initiatedBy initiatedBy,
      Se.Set Beam.idAssignedByServiceProvider idAssignedByServiceProvider,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.status status,
      Se.Set Beam.arn arn,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRefundsEntryByStripeResponse ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Payment.Interface.RefundStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
updateRefundsEntryByStripeResponse idAssignedByServiceProvider errorCode status isApiCallSuccess id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.idAssignedByServiceProvider idAssignedByServiceProvider,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.status status,
      Se.Set Beam.isApiCallSuccess isApiCallSuccess,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
