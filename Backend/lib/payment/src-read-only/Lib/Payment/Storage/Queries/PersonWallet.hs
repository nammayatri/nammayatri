{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PersonWallet where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PersonWallet
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PersonWallet as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonWallet.PersonWallet -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PersonWallet.PersonWallet] -> m ())
createMany = traverse_ create

findByPersonId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PersonWallet.PersonWallet))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq personId]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonWallet.PersonWallet -> m (Maybe Lib.Payment.Domain.Types.PersonWallet.PersonWallet))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonWallet.PersonWallet -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PersonWallet.PersonWallet {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cashAmount cashAmount,
      Se.Set Beam.cashFromPointsRedemption cashFromPointsRedemption,
      Se.Set Beam.expiredBalance expiredBalance,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.personId personId,
      Se.Set Beam.pointsAmount pointsAmount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.usableCashAmount usableCashAmount,
      Se.Set Beam.usablePointsAmount usablePointsAmount
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PersonWallet Lib.Payment.Domain.Types.PersonWallet.PersonWallet where
  fromTType' (Beam.PersonWalletT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PersonWallet.PersonWallet
          { cashAmount = cashAmount,
            cashFromPointsRedemption = cashFromPointsRedemption,
            createdAt = createdAt,
            expiredBalance = expiredBalance,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            personId = personId,
            pointsAmount = pointsAmount,
            updatedAt = updatedAt,
            usableCashAmount = usableCashAmount,
            usablePointsAmount = usablePointsAmount
          }

instance ToTType' Beam.PersonWallet Lib.Payment.Domain.Types.PersonWallet.PersonWallet where
  toTType' (Lib.Payment.Domain.Types.PersonWallet.PersonWallet {..}) = do
    Beam.PersonWalletT
      { Beam.cashAmount = cashAmount,
        Beam.cashFromPointsRedemption = cashFromPointsRedemption,
        Beam.createdAt = createdAt,
        Beam.expiredBalance = expiredBalance,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.personId = personId,
        Beam.pointsAmount = pointsAmount,
        Beam.updatedAt = updatedAt,
        Beam.usableCashAmount = usableCashAmount,
        Beam.usablePointsAmount = usablePointsAmount
      }
