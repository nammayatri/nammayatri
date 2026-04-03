{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.KioskLocation (module Storage.Queries.KioskLocation, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.KioskLocationExtra as ReExport
import qualified Domain.Types.KioskLocation
import qualified Storage.Beam.KioskLocation as Beam
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KioskLocation.KioskLocation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.KioskLocation.KioskLocation] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KioskLocation.KioskLocation -> m (Maybe Domain.Types.KioskLocation.KioskLocation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KioskLocation.KioskLocation -> m ())
updateByPrimaryKey (Domain.Types.KioskLocation.KioskLocation {..}) = do updateWithKV [Se.Set Beam.address address,
                                                                                      Se.Set Beam.contact contact,
                                                                                      Se.Set Beam.landmark landmark,
                                                                                      Se.Set Beam.latitude latitude,
                                                                                      Se.Set Beam.longitude longitude,
                                                                                      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId)] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



