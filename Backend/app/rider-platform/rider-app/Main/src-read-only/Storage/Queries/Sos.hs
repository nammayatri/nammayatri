{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Sos where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.Sos
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Sos as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.Sos.Sos -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.Sos.Sos] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Sos.Sos -> m (Maybe (Domain.Types.Sos.Sos))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByRideIdAndStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Domain.Types.Sos.SosStatus -> m (Maybe (Domain.Types.Sos.Sos))
findByRideIdAndStatus (Kernel.Types.Id.Id rideId) status = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.rideId $ Se.Eq rideId,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findByRideIdinStatusList :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Domain.Types.Sos.SosStatus] -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe (Domain.Types.Sos.Sos))
findByRideIdinStatusList status (Kernel.Types.Id.Id rideId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.status $ Se.In status,
          Se.Is Beam.rideId $ Se.Eq rideId
        ]
    ]

updateStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Sos.SosStatus -> Kernel.Types.Id.Id Domain.Types.Sos.Sos -> m ()
updateStatus status (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status $ status,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Sos.Sos -> m (Maybe (Domain.Types.Sos.Sos))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Sos.Sos -> m ()
updateByPrimaryKey Domain.Types.Sos.Sos {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flow $ flow,
      Se.Set Beam.personId $ (Kernel.Types.Id.getId personId),
      Se.Set Beam.rideId $ (Kernel.Types.Id.getId rideId),
      Se.Set Beam.status $ status,
      Se.Set Beam.ticketId $ ticketId,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.Sos Domain.Types.Sos.Sos where
  fromTType' Beam.SosT {..} = do
    pure $
      Just
        Domain.Types.Sos.Sos
          { flow = flow,
            id = Kernel.Types.Id.Id id,
            personId = Kernel.Types.Id.Id personId,
            rideId = Kernel.Types.Id.Id rideId,
            status = status,
            ticketId = ticketId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Sos Domain.Types.Sos.Sos where
  toTType' Domain.Types.Sos.Sos {..} = do
    Beam.SosT
      { Beam.flow = flow,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.status = status,
        Beam.ticketId = ticketId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
