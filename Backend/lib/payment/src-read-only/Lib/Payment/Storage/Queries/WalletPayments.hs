{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.WalletPayments where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.Wallet
import qualified Lib.Payment.Domain.Types.WalletPayments
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.WalletPayments as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.WalletPayments.WalletPayments -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.WalletPayments.WalletPayments] -> m ())
createMany = traverse_ create

findAllByWallet :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet -> m ([Lib.Payment.Domain.Types.WalletPayments.WalletPayments]))
findAllByWallet walletId = do findAllWithKV [Se.Is Beam.walletId $ Se.Eq (Kernel.Types.Id.getId walletId)]

findById ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletPayments.WalletPayments -> m (Maybe Lib.Payment.Domain.Types.WalletPayments.WalletPayments))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByOrderAndProgramAndKind ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Kernel.Prelude.Text -> Lib.Payment.Domain.Types.WalletPayments.WalletPaymentKind -> m (Maybe Lib.Payment.Domain.Types.WalletPayments.WalletPayments))
findByOrderAndProgramAndKind orderId programId kind = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getId orderId),
          Se.Is Beam.programId $ Se.Eq programId,
          Se.Is Beam.kind $ Se.Eq kind
        ]
    ]

findByOrderId ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m ([Lib.Payment.Domain.Types.WalletPayments.WalletPayments]))
findByOrderId orderId = do findAllWithKV [Se.Is Beam.orderId $ Se.Eq (Kernel.Types.Id.getId orderId)]

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
    [ Se.Set Beam.campaignId campaignId,
      Se.Set Beam.currency currency,
      Se.Set Beam.kind kind,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.orderId (Kernel.Types.Id.getId orderId),
      Se.Set Beam.personId personId,
      Se.Set Beam.points points,
      Se.Set Beam.programId programId,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.walletId (Kernel.Types.Id.getId walletId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.WalletPayments Lib.Payment.Domain.Types.WalletPayments.WalletPayments where
  fromTType' (Beam.WalletPaymentsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.WalletPayments.WalletPayments
          { campaignId = campaignId,
            createdAt = createdAt,
            currency = currency,
            id = Kernel.Types.Id.Id id,
            kind = kind,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            orderId = Kernel.Types.Id.Id orderId,
            personId = personId,
            points = points,
            programId = programId,
            status = status,
            updatedAt = updatedAt,
            walletId = Kernel.Types.Id.Id walletId
          }

instance ToTType' Beam.WalletPayments Lib.Payment.Domain.Types.WalletPayments.WalletPayments where
  toTType' (Lib.Payment.Domain.Types.WalletPayments.WalletPayments {..}) = do
    Beam.WalletPaymentsT
      { Beam.campaignId = campaignId,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.kind = kind,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.orderId = Kernel.Types.Id.getId orderId,
        Beam.personId = personId,
        Beam.points = points,
        Beam.programId = programId,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.walletId = Kernel.Types.Id.getId walletId
      }
