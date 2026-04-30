{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.WalletHistory where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Payment.Domain.Types.WalletHistory
import qualified Lib.Payment.Domain.Types.WalletPayments
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.WalletHistory as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.WalletHistory.WalletHistory -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.WalletHistory.WalletHistory] -> m ())
createMany = traverse_ create

findByWalletPaymentsAndProgramAndKindAndCampaign ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletPayments.WalletPayments -> Lib.Finance.Domain.Types.Account.CounterpartyType -> Lib.Payment.Domain.Types.WalletPayments.WalletPaymentKind -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.WalletHistory.WalletHistory))
findByWalletPaymentsAndProgramAndKindAndCampaign walletPaymentsId programType kind campaignId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.walletPaymentsId $ Se.Eq (Kernel.Types.Id.getId walletPaymentsId),
          Se.Is Beam.programType $ Se.Eq programType,
          Se.Is Beam.kind $ Se.Eq kind,
          Se.Is Beam.campaignId $ Se.Eq campaignId
        ]
    ]

updatePointsAndBenefit ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletHistory.WalletHistory -> m ())
updatePointsAndBenefit points reversedPoints benefitValue id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.points points,
      Se.Set Beam.reversedPoints reversedPoints,
      Se.Set Beam.benefitValue benefitValue,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletHistory.WalletHistory -> m (Maybe Lib.Payment.Domain.Types.WalletHistory.WalletHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.WalletHistory.WalletHistory -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.WalletHistory.WalletHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.benefitValue benefitValue,
      Se.Set Beam.campaignId campaignId,
      Se.Set Beam.domainEntityId domainEntityId,
      Se.Set Beam.kind kind,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.points points,
      Se.Set Beam.programType programType,
      Se.Set Beam.reversedPoints reversedPoints,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.walletId (Kernel.Types.Id.getId walletId),
      Se.Set Beam.walletPaymentsId (Kernel.Types.Id.getId walletPaymentsId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.WalletHistory Lib.Payment.Domain.Types.WalletHistory.WalletHistory where
  fromTType' (Beam.WalletHistoryT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.WalletHistory.WalletHistory
          { benefitValue = benefitValue,
            campaignId = campaignId,
            createdAt = createdAt,
            domainEntityId = domainEntityId,
            id = Kernel.Types.Id.Id id,
            kind = kind,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            points = points,
            programType = programType,
            reversedPoints = reversedPoints,
            updatedAt = updatedAt,
            walletId = Kernel.Types.Id.Id walletId,
            walletPaymentsId = Kernel.Types.Id.Id walletPaymentsId
          }

instance ToTType' Beam.WalletHistory Lib.Payment.Domain.Types.WalletHistory.WalletHistory where
  toTType' (Lib.Payment.Domain.Types.WalletHistory.WalletHistory {..}) = do
    Beam.WalletHistoryT
      { Beam.benefitValue = benefitValue,
        Beam.campaignId = campaignId,
        Beam.createdAt = createdAt,
        Beam.domainEntityId = domainEntityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.kind = kind,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.points = points,
        Beam.programType = programType,
        Beam.reversedPoints = reversedPoints,
        Beam.updatedAt = updatedAt,
        Beam.walletId = Kernel.Types.Id.getId walletId,
        Beam.walletPaymentsId = Kernel.Types.Id.getId walletPaymentsId
      }
