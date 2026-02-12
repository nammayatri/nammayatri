{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.Account where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Finance.Storage.Beam.Account as Beam
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Account.Account -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.Account.Account] -> m ())
createMany = traverse_ create

findByCounterparty ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.Account.Account]))
findByCounterparty counterpartyType counterpartyId = do findAllWithKV [Se.And [Se.Is Beam.counterpartyType $ Se.Eq counterpartyType, Se.Is Beam.counterpartyId $ Se.Eq counterpartyId]]

findByCounterpartyAndType ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Finance.Domain.Types.Account.AccountType -> Kernel.Types.Common.Currency -> m (Maybe Lib.Finance.Domain.Types.Account.Account))
findByCounterpartyAndType counterpartyType counterpartyId accountType currency = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.counterpartyType $ Se.Eq counterpartyType,
          Se.Is Beam.counterpartyId $ Se.Eq counterpartyId,
          Se.Is Beam.accountType $ Se.Eq accountType,
          Se.Is Beam.currency $ Se.Eq currency
        ]
    ]

findById :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account -> m (Maybe Lib.Finance.Domain.Types.Account.Account))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateBalance :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account -> m ())
updateBalance balance id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.balance balance, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Account.AccountStatus -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account -> m ())
updateStatus status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account -> m (Maybe Lib.Finance.Domain.Types.Account.Account))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Account.Account -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.Account.Account {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountCategory accountCategory,
      Se.Set Beam.accountType accountType,
      Se.Set Beam.balance balance,
      Se.Set Beam.counterpartyId counterpartyId,
      Se.Set Beam.counterpartyType counterpartyType,
      Se.Set Beam.currency currency,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Account Lib.Finance.Domain.Types.Account.Account where
  fromTType' (Beam.AccountT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.Account.Account
          { accountCategory = accountCategory,
            accountType = accountType,
            balance = balance,
            counterpartyId = counterpartyId,
            counterpartyType = counterpartyType,
            createdAt = createdAt,
            currency = currency,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Account Lib.Finance.Domain.Types.Account.Account where
  toTType' (Lib.Finance.Domain.Types.Account.Account {..}) = do
    Beam.AccountT
      { Beam.accountCategory = accountCategory,
        Beam.accountType = accountType,
        Beam.balance = balance,
        Beam.counterpartyId = counterpartyId,
        Beam.counterpartyType = counterpartyType,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
