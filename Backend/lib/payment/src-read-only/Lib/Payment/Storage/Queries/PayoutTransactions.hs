{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutTransactions where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutTransactions
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutTransactions as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions -> m (Maybe Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTransactionRef :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions))
findByTransactionRef transactionRef = do findOneWithKV [Se.Is Beam.transactionRef $ Se.Eq transactionRef]

updatePayoutTransactionStatus :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updatePayoutTransactionStatus status transactionRef = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.transactionRef $ Se.Eq transactionRef]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions -> Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions))
findByPrimaryKey id transactionRef = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.transactionRef $ Se.Eq transactionRef]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.fulfillmentMethod fulfillmentMethod,
      Se.Set Beam.gateWayRefId gateWayRefId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.payoutOrderId (Kernel.Types.Id.getId payoutOrderId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.transactionRef $ Se.Eq transactionRef]]

instance FromTType' Beam.PayoutTransactions Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions where
  fromTType' (Beam.PayoutTransactionsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions
          { amount = amount,
            createdAt = createdAt,
            fulfillmentMethod = fulfillmentMethod,
            gateWayRefId = gateWayRefId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            payoutOrderId = Kernel.Types.Id.Id payoutOrderId,
            status = status,
            transactionRef = transactionRef,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutTransactions Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions where
  toTType' (Lib.Payment.Domain.Types.PayoutTransactions.PayoutTransactions {..}) = do
    Beam.PayoutTransactionsT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.fulfillmentMethod = fulfillmentMethod,
        Beam.gateWayRefId = gateWayRefId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.payoutOrderId = Kernel.Types.Id.getId payoutOrderId,
        Beam.status = status,
        Beam.transactionRef = transactionRef,
        Beam.updatedAt = updatedAt
      }
