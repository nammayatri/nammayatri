{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.Refunds where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.Refunds
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.Refunds as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
create = createWithKV

findAllByOrderId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m [Lib.Payment.Domain.Types.Refunds.Refunds])
findAllByOrderId orderId = do findAllWithKV [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getShortId orderId)]

findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m (Maybe Lib.Payment.Domain.Types.Refunds.Refunds))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByShortId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.Refunds.Refunds -> m (Maybe Lib.Payment.Domain.Types.Refunds.Refunds))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

updateRefundsEntryByResponse ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Payment.Interface.RefundStatus -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
updateRefundsEntryByResponse initiatedBy idAssignedByServiceProvider errorMessage errorCode status id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.initiatedBy initiatedBy,
      Se.Set Beam.idAssignedByServiceProvider idAssignedByServiceProvider,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.Refunds Lib.Payment.Domain.Types.Refunds.Refunds where
  fromTType' (Beam.RefundsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.Refunds.Refunds
          { createdAt = createdAt,
            errorCode = errorCode,
            errorMessage = errorMessage,
            id = Kernel.Types.Id.Id id,
            idAssignedByServiceProvider = idAssignedByServiceProvider,
            initiatedBy = initiatedBy,
            merchantId = merchantId,
            orderId = Kernel.Types.Id.ShortId orderId,
            refundAmount = refundAmount,
            shortId = Kernel.Types.Id.ShortId shortId,
            split = split >>= Kernel.Utils.JSON.valueToMaybe,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Refunds Lib.Payment.Domain.Types.Refunds.Refunds where
  toTType' (Lib.Payment.Domain.Types.Refunds.Refunds {..}) = do
    Beam.RefundsT
      { Beam.createdAt = createdAt,
        Beam.errorCode = errorCode,
        Beam.errorMessage = errorMessage,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idAssignedByServiceProvider = idAssignedByServiceProvider,
        Beam.initiatedBy = initiatedBy,
        Beam.merchantId = merchantId,
        Beam.orderId = Kernel.Types.Id.getShortId orderId,
        Beam.refundAmount = refundAmount,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.split = split >>= Just . Data.Aeson.toJSON,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
