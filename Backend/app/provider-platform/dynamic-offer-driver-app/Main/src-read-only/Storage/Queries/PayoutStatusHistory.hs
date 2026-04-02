{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutStatusHistory where

import qualified Domain.Types.PayoutStatusHistory
import qualified Domain.Types.ScheduledPayout
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutStatusHistory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutStatusHistory.PayoutStatusHistory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PayoutStatusHistory.PayoutStatusHistory] -> m ())
createMany = traverse_ create

findByScheduledPayoutId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.ScheduledPayout.ScheduledPayout -> m ([Domain.Types.PayoutStatusHistory.PayoutStatusHistory]))
findByScheduledPayoutId limit offset scheduledPayoutId = do findAllWithOptionsKV [Se.Is Beam.scheduledPayoutId $ Se.Eq (Kernel.Types.Id.getId scheduledPayoutId)] (Se.Desc Beam.createdAt) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PayoutStatusHistory.PayoutStatusHistory -> m (Maybe Domain.Types.PayoutStatusHistory.PayoutStatusHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutStatusHistory.PayoutStatusHistory -> m ())
updateByPrimaryKey (Domain.Types.PayoutStatusHistory.PayoutStatusHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.message message,
      Se.Set Beam.scheduledPayoutId (Kernel.Types.Id.getId scheduledPayoutId),
      Se.Set Beam.status status,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PayoutStatusHistory Domain.Types.PayoutStatusHistory.PayoutStatusHistory where
  fromTType' (Beam.PayoutStatusHistoryT {..}) = do
    pure $
      Just
        Domain.Types.PayoutStatusHistory.PayoutStatusHistory
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            message = message,
            scheduledPayoutId = Kernel.Types.Id.Id scheduledPayoutId,
            status = status,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutStatusHistory Domain.Types.PayoutStatusHistory.PayoutStatusHistory where
  toTType' (Domain.Types.PayoutStatusHistory.PayoutStatusHistory {..}) = do
    Beam.PayoutStatusHistoryT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.message = message,
        Beam.scheduledPayoutId = Kernel.Types.Id.getId scheduledPayoutId,
        Beam.status = status,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
