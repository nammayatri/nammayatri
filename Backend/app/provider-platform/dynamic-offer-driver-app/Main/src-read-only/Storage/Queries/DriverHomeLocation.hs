{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.DriverHomeLocation (module Storage.Queries.DriverHomeLocation, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.DriverHomeLocationExtra as ReExport
import qualified Domain.Types.DriverHomeLocation
import qualified Storage.Beam.DriverHomeLocation as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverHomeLocation.DriverHomeLocation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverHomeLocation.DriverHomeLocation] -> m ())
createMany = traverse_ create
deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverHomeLocation.DriverHomeLocation -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.DriverHomeLocation.DriverHomeLocation]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverHomeLocation.DriverHomeLocation -> m (Maybe Domain.Types.DriverHomeLocation.DriverHomeLocation))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.DriverHomeLocation.DriverHomeLocation -> m (Maybe Domain.Types.DriverHomeLocation.DriverHomeLocation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverHomeLocation.DriverHomeLocation -> m ())
updateByPrimaryKey (Domain.Types.DriverHomeLocation.DriverHomeLocation {..}) = do {_now <- getCurrentTime;
                                                                                   updateWithKV [Se.Set Beam.address address,
                                                                                                 Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
                                                                                                 Se.Set Beam.lat lat,
                                                                                                 Se.Set Beam.lon lon,
                                                                                                 Se.Set Beam.tag tag,
                                                                                                 Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



