{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationDuesDetails (module Storage.Queries.CancellationDuesDetails, module ReExport) where

import qualified Domain.Types.CancellationDuesDetails
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationDuesDetails as Beam
import Storage.Queries.CancellationDuesDetailsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationDuesDetails.CancellationDuesDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationDuesDetails.CancellationDuesDetails] -> m ())
createMany = traverse_ create

findByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.CancellationDuesDetails.CancellationDuesDetails))
findByRideId rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

updatePaymentStatusByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationDuesDetails.CancellationDuesPaymentStatus -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updatePaymentStatusByRideId paymentStatus rideId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.paymentStatus paymentStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CancellationDuesDetails.CancellationDuesDetails -> m (Maybe Domain.Types.CancellationDuesDetails.CancellationDuesDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationDuesDetails.CancellationDuesDetails -> m ())
updateByPrimaryKey (Domain.Types.CancellationDuesDetails.CancellationDuesDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cancellationAmount cancellationAmount,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.paymentStatus paymentStatus,
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
