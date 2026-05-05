{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.Wallet where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Payment.Domain.Types.Wallet
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.Wallet as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.Wallet.Wallet -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.Wallet.Wallet] -> m ())
createMany = traverse_ create

findByPersonAndProgram ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Lib.Finance.Domain.Types.Account.CounterpartyType -> m (Maybe Lib.Payment.Domain.Types.Wallet.Wallet))
findByPersonAndProgram personId programType = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq personId, Se.Is Beam.programType $ Se.Eq programType]]

syncFromLoyaltyInfo ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet -> m ())
syncFromLoyaltyInfo currentAvailablePoints availableBalance lifetimeEarned lifetimeBurned topupEarned cashbackEarned id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currentAvailablePoints currentAvailablePoints,
      Se.Set Beam.availableBalance availableBalance,
      Se.Set Beam.lifetimeEarned lifetimeEarned,
      Se.Set Beam.lifetimeBurned lifetimeBurned,
      Se.Set Beam.topupEarned topupEarned,
      Se.Set Beam.cashbackEarned cashbackEarned,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAggregatesOnBurn ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet -> m ())
updateAggregatesOnBurn availableBalance lifetimeBurned id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.availableBalance availableBalance, Se.Set Beam.lifetimeBurned lifetimeBurned, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAggregatesOnEarn ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet -> m ())
updateAggregatesOnEarn availableBalance lifetimeEarned topupEarned cashbackEarned id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.availableBalance availableBalance,
      Se.Set Beam.lifetimeEarned lifetimeEarned,
      Se.Set Beam.topupEarned topupEarned,
      Se.Set Beam.cashbackEarned cashbackEarned,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Wallet.Wallet -> m (Maybe Lib.Payment.Domain.Types.Wallet.Wallet))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.Wallet.Wallet -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.Wallet.Wallet {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountId (Kernel.Types.Id.getId accountId),
      Se.Set Beam.availableBalance availableBalance,
      Se.Set Beam.cashbackEarned cashbackEarned,
      Se.Set Beam.currency currency,
      Se.Set Beam.currentAvailablePoints currentAvailablePoints,
      Se.Set Beam.lifetimeBurned lifetimeBurned,
      Se.Set Beam.lifetimeEarned lifetimeEarned,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.personId personId,
      Se.Set Beam.programId programId,
      Se.Set Beam.programType programType,
      Se.Set Beam.topupEarned topupEarned,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Wallet Lib.Payment.Domain.Types.Wallet.Wallet where
  fromTType' (Beam.WalletT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.Wallet.Wallet
          { accountId = Kernel.Types.Id.Id accountId,
            availableBalance = availableBalance,
            cashbackEarned = cashbackEarned,
            createdAt = createdAt,
            currency = currency,
            currentAvailablePoints = currentAvailablePoints,
            id = Kernel.Types.Id.Id id,
            lifetimeBurned = lifetimeBurned,
            lifetimeEarned = lifetimeEarned,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            personId = personId,
            programId = programId,
            programType = programType,
            topupEarned = topupEarned,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Wallet Lib.Payment.Domain.Types.Wallet.Wallet where
  toTType' (Lib.Payment.Domain.Types.Wallet.Wallet {..}) = do
    Beam.WalletT
      { Beam.accountId = Kernel.Types.Id.getId accountId,
        Beam.availableBalance = availableBalance,
        Beam.cashbackEarned = cashbackEarned,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.currentAvailablePoints = currentAvailablePoints,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lifetimeBurned = lifetimeBurned,
        Beam.lifetimeEarned = lifetimeEarned,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.personId = personId,
        Beam.programId = programId,
        Beam.programType = programType,
        Beam.topupEarned = topupEarned,
        Beam.updatedAt = updatedAt
      }
