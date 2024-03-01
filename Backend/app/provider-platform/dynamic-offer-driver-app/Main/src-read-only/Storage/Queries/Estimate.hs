{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Estimate where

import qualified Domain.Types.Common
import qualified Domain.Types.Estimate
import qualified Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.Vehicle
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as Beam
import qualified Storage.CachedQueries.FarePolicy
import qualified Storage.Queries.FareParameters

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.Estimate.Estimate -> m ()
create tbl = do
  Kernel.Prelude.whenJust tbl.fareParams Storage.Queries.FareParameters.create

  createWithKV tbl

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.Estimate.Estimate] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe (Domain.Types.Estimate.Estimate))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe (Domain.Types.Estimate.Estimate))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Estimate.Estimate -> m ()
updateByPrimaryKey Domain.Types.Estimate.Estimate {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.fareParamsId $ (Kernel.Types.Id.getId . (.id) <$>) fareParams,
      Se.Set Beam.farePolicyId $ (Kernel.Types.Id.getId . (.id) <$>) farePolicy,
      Se.Set Beam.isScheduled $ Kernel.Prelude.Just isScheduled,
      Se.Set Beam.maxFare maxFare,
      Se.Set Beam.minFare minFare,
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.tripCategory $ Kernel.Prelude.Just tripCategory,
      Se.Set Beam.updatedAt $ Kernel.Prelude.Just updatedAt,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  fromTType' Beam.EstimateT {..} = do
    fareParams' <- maybe (pure Nothing) (Storage.Queries.FareParameters.findById . Kernel.Types.Id.Id) fareParamsId
    farePolicy' <- maybe (pure Nothing) ((Storage.CachedQueries.FarePolicy.findById Nothing) . Kernel.Types.Id.Id) farePolicyId
    pure $
      Just
        Domain.Types.Estimate.Estimate
          { createdAt = createdAt,
            estimatedDistance = estimatedDistance,
            fareParams = fareParams',
            farePolicy = farePolicy',
            id = Kernel.Types.Id.Id id,
            isScheduled = Kernel.Prelude.fromMaybe Kernel.Prelude.False isScheduled,
            maxFare = maxFare,
            minFare = minFare,
            requestId = Kernel.Types.Id.Id requestId,
            specialLocationTag = specialLocationTag,
            tripCategory = Kernel.Prelude.fromMaybe (Domain.Types.Common.OneWay Domain.Types.Common.OneWayOnDemandDynamicOffer) tripCategory,
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' Domain.Types.Estimate.Estimate {..} = do
    Beam.EstimateT
      { Beam.createdAt = createdAt,
        Beam.estimatedDistance = estimatedDistance,
        Beam.fareParamsId = (Kernel.Types.Id.getId . (.id) <$>) fareParams,
        Beam.farePolicyId = (Kernel.Types.Id.getId . (.id) <$>) farePolicy,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isScheduled = Kernel.Prelude.Just isScheduled,
        Beam.maxFare = maxFare,
        Beam.minFare = minFare,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.specialLocationTag = specialLocationTag,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.vehicleVariant = vehicleVariant
      }
