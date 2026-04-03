{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.CancellationDuesDetails (module Storage.Queries.CancellationDuesDetails, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.CancellationDuesDetailsExtra as ReExport
import qualified Domain.Types.CancellationDuesDetails
import qualified Storage.Beam.CancellationDuesDetails as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Ride
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationDuesDetails.CancellationDuesDetails -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationDuesDetails.CancellationDuesDetails] -> m ())
createMany = traverse_ create
findByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.CancellationDuesDetails.CancellationDuesDetails))
findByRideId rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]
updatePaymentStatusByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationDuesDetails.CancellationDuesPaymentStatus -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updatePaymentStatusByRideId paymentStatus rideId = do {_now <- getCurrentTime;
                                                       updateOneWithKV [Se.Set Beam.paymentStatus paymentStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.CancellationDuesDetails.CancellationDuesDetails -> m (Maybe Domain.Types.CancellationDuesDetails.CancellationDuesDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationDuesDetails.CancellationDuesDetails -> m ())
updateByPrimaryKey (Domain.Types.CancellationDuesDetails.CancellationDuesDetails {..}) = do {_now <- getCurrentTime;
                                                                                             updateWithKV [Se.Set Beam.cancellationAmount cancellationAmount,
                                                                                                           Se.Set Beam.currency (Kernel.Prelude.Just currency),
                                                                                                           Se.Set Beam.paymentStatus paymentStatus,
                                                                                                           Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
                                                                                                           Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
                                                                                                           Se.Set Beam.updatedAt _now,
                                                                                                           Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                                           Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



