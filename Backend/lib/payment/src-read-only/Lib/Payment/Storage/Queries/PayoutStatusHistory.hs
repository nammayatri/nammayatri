{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutStatusHistory where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutStatusHistory
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutStatusHistory as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory] -> m ())
createMany = traverse_ create

findByScheduledPayoutId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m ([Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory]))
findByScheduledPayoutId limit offset scheduledPayoutId = do findAllWithOptionsKV [Se.Is Beam.scheduledPayoutId $ Se.Eq scheduledPayoutId] (Se.Desc Beam.createdAt) limit offset

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory -> m (Maybe Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.message message,
      Se.Set Beam.scheduledPayoutId scheduledPayoutId,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PayoutStatusHistory Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory where
  fromTType' (Beam.PayoutStatusHistoryT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            message = message,
            scheduledPayoutId = scheduledPayoutId,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutStatusHistory Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory where
  toTType' (Lib.Payment.Domain.Types.PayoutStatusHistory.PayoutStatusHistory {..}) = do
    Beam.PayoutStatusHistoryT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.message = message,
        Beam.scheduledPayoutId = scheduledPayoutId,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
