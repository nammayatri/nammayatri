{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.WalletPayments where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.WalletPayments
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.WalletPayments as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.WalletPayments.WalletPayments -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.WalletPayments.WalletPayments] -> m ())
createMany = traverse_ create

findByOrderId ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m (Maybe Lib.Payment.Domain.Types.WalletPayments.WalletPayments))
findByOrderId orderId = do findOneWithKV [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getId orderId)]

updateStatus ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.WalletPayments.WalletPaymentStatus -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletPayments.WalletPayments -> m ())
updateStatus status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletPayments.WalletPayments -> m (Maybe Lib.Payment.Domain.Types.WalletPayments.WalletPayments))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.WalletPayments.WalletPayments -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.WalletPayments.WalletPayments {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.domainEntityId domainEntityId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.orderId (Kernel.Types.Id.getId orderId),
      Se.Set Beam.personId personId,
      Se.Set Beam.status status,
      Se.Set Beam.totalBurned totalBurned,
      Se.Set Beam.totalEarned totalEarned,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.WalletPayments Lib.Payment.Domain.Types.WalletPayments.WalletPayments where
  fromTType' (Beam.WalletPaymentsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.WalletPayments.WalletPayments
          { createdAt = createdAt,
            currency = currency,
            domainEntityId = domainEntityId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            orderId = Kernel.Types.Id.Id orderId,
            personId = personId,
            status = status,
            totalBurned = totalBurned,
            totalEarned = totalEarned,
            updatedAt = updatedAt
          }

instance ToTType' Beam.WalletPayments Lib.Payment.Domain.Types.WalletPayments.WalletPayments where
  toTType' (Lib.Payment.Domain.Types.WalletPayments.WalletPayments {..}) = do
    Beam.WalletPaymentsT
      { Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.domainEntityId = domainEntityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.orderId = Kernel.Types.Id.getId orderId,
        Beam.personId = personId,
        Beam.status = status,
        Beam.totalBurned = totalBurned,
        Beam.totalEarned = totalEarned,
        Beam.updatedAt = updatedAt
      }
