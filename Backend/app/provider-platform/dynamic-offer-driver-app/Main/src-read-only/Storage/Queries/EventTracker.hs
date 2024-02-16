{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EventTracker where

import qualified Domain.Types.EventTracker
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EventTracker as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.EventTracker.EventTracker -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.EventTracker.EventTracker] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.EventTracker.EventTracker -> m (Maybe (Domain.Types.EventTracker.EventTracker))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.EventTracker.EventTracker -> m (Maybe (Domain.Types.EventTracker.EventTracker))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.EventTracker.EventTracker -> m ()
updateByPrimaryKey Domain.Types.EventTracker.EventTracker {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.entity $ entity,
      Se.Set Beam.entityFieldName $ entityFieldName,
      Se.Set Beam.entityPrimaryId $ entityPrimaryId,
      Se.Set Beam.eventName $ eventName,
      Se.Set Beam.fromState $ fromState,
      Se.Set Beam.reason $ reason,
      Se.Set Beam.subscriptionServiceName $ subscriptionServiceName,
      Se.Set Beam.toState $ toState,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.EventTracker Domain.Types.EventTracker.EventTracker where
  fromTType' Beam.EventTrackerT {..} = do
    pure $
      Just
        Domain.Types.EventTracker.EventTracker
          { createdAt = createdAt,
            entity = entity,
            entityFieldName = entityFieldName,
            entityPrimaryId = entityPrimaryId,
            eventName = eventName,
            fromState = fromState,
            id = Kernel.Types.Id.Id id,
            reason = reason,
            subscriptionServiceName = subscriptionServiceName,
            toState = toState,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EventTracker Domain.Types.EventTracker.EventTracker where
  toTType' Domain.Types.EventTracker.EventTracker {..} = do
    Beam.EventTrackerT
      { Beam.createdAt = createdAt,
        Beam.entity = entity,
        Beam.entityFieldName = entityFieldName,
        Beam.entityPrimaryId = entityPrimaryId,
        Beam.eventName = eventName,
        Beam.fromState = fromState,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reason = reason,
        Beam.subscriptionServiceName = subscriptionServiceName,
        Beam.toState = toState,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
