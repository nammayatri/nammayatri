{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.SavedReqLocation (module Storage.Queries.SavedReqLocation, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.SavedReqLocationExtra as ReExport
import qualified Domain.Types.SavedReqLocation
import qualified Storage.Beam.SavedReqLocation as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SavedReqLocation.SavedReqLocation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SavedReqLocation.SavedReqLocation] -> m ())
createMany = traverse_ create
deleteAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteAllByRiderId riderId = do deleteWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]



