{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.WalletRewardPosting where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PersonWallet
import qualified Lib.Payment.Domain.Types.WalletRewardPosting
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.WalletRewardPosting as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting] -> m ())
createMany = traverse_ create

findAllByWalletId ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonWallet.PersonWallet -> m ([Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting]))
findAllByWalletId walletId = do findAllWithKV [Se.Is Beam.walletId $ Se.Eq (Kernel.Types.Id.getId walletId)]

findByWalletIdAndStatus ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonWallet.PersonWallet -> Lib.Payment.Domain.Types.WalletRewardPosting.WalletPostingStatus -> m (Maybe Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting))
findByWalletIdAndStatus walletId status = do findOneWithKV [Se.And [Se.Is Beam.walletId $ Se.Eq (Kernel.Types.Id.getId walletId), Se.Is Beam.status $ Se.Eq status]]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting -> m (Maybe Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cashAmount cashAmount,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.pointsAmount pointsAmount,
      Se.Set Beam.postingType postingType,
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.walletId (Kernel.Types.Id.getId walletId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.WalletRewardPosting Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting where
  fromTType' (Beam.WalletRewardPostingT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting
          { cashAmount = cashAmount,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            pointsAmount = pointsAmount,
            postingType = postingType,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            updatedAt = updatedAt,
            walletId = Kernel.Types.Id.Id walletId
          }

instance ToTType' Beam.WalletRewardPosting Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting where
  toTType' (Lib.Payment.Domain.Types.WalletRewardPosting.WalletRewardPosting {..}) = do
    Beam.WalletRewardPostingT
      { Beam.cashAmount = cashAmount,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.pointsAmount = pointsAmount,
        Beam.postingType = postingType,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.walletId = Kernel.Types.Id.getId walletId
      }
