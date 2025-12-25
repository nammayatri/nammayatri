{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SavedReqLocation (module Storage.Queries.SavedReqLocation, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.SavedReqLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SavedReqLocation as Beam
import Storage.Queries.SavedReqLocationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SavedReqLocation.SavedReqLocation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SavedReqLocation.SavedReqLocation] -> m ())
createMany = traverse_ create

deleteAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteAllByRiderId riderId = do deleteWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]
