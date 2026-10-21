{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BillingModelSwitchHistory where

import qualified Domain.Types.BillingModelSwitchHistory
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BillingModelSwitchHistory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory] -> m ())
createMany = traverse_ create

findLatestByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory]))
findLatestByDriverId limit offset driverId = do findAllWithOptionsKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)] (Se.Desc Beam.createdAt) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory -> m (Maybe Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory -> m ())
updateByPrimaryKey (Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acknowledgedUnusedCredits acknowledgedUnusedCredits,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.fromModel fromModel,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.requestedBy requestedBy,
      Se.Set Beam.source source,
      Se.Set Beam.toModel toModel,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BillingModelSwitchHistory Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory where
  fromTType' (Beam.BillingModelSwitchHistoryT {..}) = do
    pure $
      Just
        Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory
          { acknowledgedUnusedCredits = acknowledgedUnusedCredits,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            fromModel = fromModel,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            requestedBy = requestedBy,
            source = source,
            toModel = toModel,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BillingModelSwitchHistory Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory where
  toTType' (Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory {..}) = do
    Beam.BillingModelSwitchHistoryT
      { Beam.acknowledgedUnusedCredits = acknowledgedUnusedCredits,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.fromModel = fromModel,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.requestedBy = requestedBy,
        Beam.source = source,
        Beam.toModel = toModel,
        Beam.updatedAt = updatedAt
      }
