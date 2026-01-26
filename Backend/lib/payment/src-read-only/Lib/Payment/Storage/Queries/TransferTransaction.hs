{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.TransferTransaction where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Stripe.Types.Transfer
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.TransferTransaction
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.TransferTransaction as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.TransferTransaction.TransferTransaction -> m ())
create = createWithKV

updateTranferEntryByStripeResponse ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Payment.Stripe.Types.Transfer.TransferStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.TransferTransaction.TransferTransaction -> m ())
updateTranferEntryByStripeResponse idAssignedByServiceProvider errorCode errorMessage status isApiCallSuccess id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.idAssignedByServiceProvider idAssignedByServiceProvider,
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.status status,
      Se.Set Beam.isApiCallSuccess isApiCallSuccess,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.TransferTransaction Lib.Payment.Domain.Types.TransferTransaction.TransferTransaction where
  fromTType' (Beam.TransferTransactionT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.TransferTransaction.TransferTransaction
          { amount = amount,
            currency = currency,
            description = description,
            destinationAccountId = destinationAccountId,
            entityId = Kernel.Types.Id.Id entityId,
            entityName = entityName,
            errorCode = errorCode,
            errorMessage = errorMessage,
            id = Kernel.Types.Id.Id id,
            idAssignedByServiceProvider = idAssignedByServiceProvider,
            isApiCallSuccess = isApiCallSuccess,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            responseDump = responseDump,
            senderAccountId = senderAccountId,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TransferTransaction Lib.Payment.Domain.Types.TransferTransaction.TransferTransaction where
  toTType' (Lib.Payment.Domain.Types.TransferTransaction.TransferTransaction {..}) = do
    Beam.TransferTransactionT
      { Beam.amount = amount,
        Beam.currency = currency,
        Beam.description = description,
        Beam.destinationAccountId = destinationAccountId,
        Beam.entityId = Kernel.Types.Id.getId entityId,
        Beam.entityName = entityName,
        Beam.errorCode = errorCode,
        Beam.errorMessage = errorMessage,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idAssignedByServiceProvider = idAssignedByServiceProvider,
        Beam.isApiCallSuccess = isApiCallSuccess,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.responseDump = responseDump,
        Beam.senderAccountId = senderAccountId,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
