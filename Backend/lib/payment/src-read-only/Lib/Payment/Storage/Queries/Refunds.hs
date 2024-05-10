{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.Refunds where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Refunds
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.Refunds as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
create = createWithKV

findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m (Maybe Lib.Payment.Domain.Types.Refunds.Refunds))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByShortId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.Refunds.Refunds))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq shortId]

updateRefundsEntryByResponse ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Payment.Interface.RefundStatus -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds -> m ())
updateRefundsEntryByResponse initiatedBy idAssignedByServiceProvider errorMessage errorCode status (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.initiatedBy initiatedBy,
      Se.Set Beam.idAssignedByServiceProvider idAssignedByServiceProvider,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq id]

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
            orderId = Kernel.Types.Id.Id orderId,
            refundAmount = refundAmount,
            shortId = shortId,
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
        Beam.orderId = Kernel.Types.Id.getId orderId,
        Beam.refundAmount = refundAmount,
        Beam.shortId = shortId,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
