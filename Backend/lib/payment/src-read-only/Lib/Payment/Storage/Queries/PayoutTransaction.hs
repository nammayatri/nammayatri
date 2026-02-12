{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutTransaction where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutTransaction
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutTransaction as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction -> m (Maybe Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTransactionRef :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction))
findByTransactionRef transactionRef = do findOneWithKV [Se.Is Beam.transactionRef $ Se.Eq transactionRef]

updatePayoutTransactionStatus :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updatePayoutTransactionStatus status transactionRef = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.transactionRef $ Se.Eq transactionRef]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction -> Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction))
findByPrimaryKey id transactionRef = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.transactionRef $ Se.Eq transactionRef]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency (((Kernel.Prelude.Just . (.currency))) amount),
      Se.Set Beam.price ((.amount) amount),
      Se.Set Beam.fulfillmentMethod fulfillmentMethod,
      Se.Set Beam.gateWayRefId gateWayRefId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.payoutOrderId (Kernel.Types.Id.getId payoutOrderId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.transactionRef $ Se.Eq transactionRef]]

instance FromTType' Beam.PayoutTransaction Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction where
  fromTType' (Beam.PayoutTransactionT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction
          { amount = Kernel.Types.Common.mkPrice currency price,
            createdAt = createdAt,
            fulfillmentMethod = fulfillmentMethod,
            gateWayRefId = gateWayRefId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            payoutOrderId = Kernel.Types.Id.Id payoutOrderId,
            status = status,
            transactionRef = transactionRef,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutTransaction Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction where
  toTType' (Lib.Payment.Domain.Types.PayoutTransaction.PayoutTransaction {..}) = do
    Beam.PayoutTransactionT
      { Beam.currency = ((Kernel.Prelude.Just . (.currency))) amount,
        Beam.price = (.amount) amount,
        Beam.createdAt = createdAt,
        Beam.fulfillmentMethod = fulfillmentMethod,
        Beam.gateWayRefId = gateWayRefId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.payoutOrderId = Kernel.Types.Id.getId payoutOrderId,
        Beam.status = status,
        Beam.transactionRef = transactionRef,
        Beam.updatedAt = updatedAt
      }
