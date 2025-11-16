{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationFarePolicy where

import qualified Domain.Types.CancellationFarePolicy
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationFarePolicy as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationFarePolicy.CancellationFarePolicy -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationFarePolicy.CancellationFarePolicy] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CancellationFarePolicy.CancellationFarePolicy -> m (Maybe Domain.Types.CancellationFarePolicy.CancellationFarePolicy))
findById id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CancellationFarePolicy.CancellationFarePolicy -> m (Maybe Domain.Types.CancellationFarePolicy.CancellationFarePolicy))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationFarePolicy.CancellationFarePolicy -> m ())
updateByPrimaryKey (Domain.Types.CancellationFarePolicy.CancellationFarePolicy {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.description description,
      Se.Set Beam.freeCancellationTimeSeconds freeCancellationTimeSeconds,
      Se.Set Beam.maxCancellationCharge maxCancellationCharge,
      Se.Set Beam.maxWaitingTimeAtPickupSeconds maxWaitingTimeAtPickupSeconds,
      Se.Set Beam.minCancellationCharge minCancellationCharge,
      Se.Set Beam.perMetreCancellationCharge perMetreCancellationCharge,
      Se.Set Beam.perMinuteCancellationCharge perMinuteCancellationCharge,
      Se.Set Beam.percentageOfRideFareToBeCharged percentageOfRideFareToBeCharged,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CancellationFarePolicy Domain.Types.CancellationFarePolicy.CancellationFarePolicy where
  fromTType' (Beam.CancellationFarePolicyT {..}) = do
    pure $
      Just
        Domain.Types.CancellationFarePolicy.CancellationFarePolicy
          { currency = currency,
            description = description,
            freeCancellationTimeSeconds = freeCancellationTimeSeconds,
            id = Kernel.Types.Id.Id id,
            maxCancellationCharge = maxCancellationCharge,
            maxWaitingTimeAtPickupSeconds = maxWaitingTimeAtPickupSeconds,
            minCancellationCharge = minCancellationCharge,
            perMetreCancellationCharge = perMetreCancellationCharge,
            perMinuteCancellationCharge = perMinuteCancellationCharge,
            percentageOfRideFareToBeCharged = percentageOfRideFareToBeCharged,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CancellationFarePolicy Domain.Types.CancellationFarePolicy.CancellationFarePolicy where
  toTType' (Domain.Types.CancellationFarePolicy.CancellationFarePolicy {..}) = do
    Beam.CancellationFarePolicyT
      { Beam.currency = currency,
        Beam.description = description,
        Beam.freeCancellationTimeSeconds = freeCancellationTimeSeconds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxCancellationCharge = maxCancellationCharge,
        Beam.maxWaitingTimeAtPickupSeconds = maxWaitingTimeAtPickupSeconds,
        Beam.minCancellationCharge = minCancellationCharge,
        Beam.perMetreCancellationCharge = perMetreCancellationCharge,
        Beam.perMinuteCancellationCharge = perMinuteCancellationCharge,
        Beam.percentageOfRideFareToBeCharged = percentageOfRideFareToBeCharged,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
