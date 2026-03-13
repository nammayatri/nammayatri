{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateRouteStop (module Storage.Queries.CorporateRouteStop, module ReExport) where

import qualified Domain.Types.CorporateRoute
import qualified Domain.Types.CorporateRouteStop
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateRouteStop as Beam
import Storage.Queries.CorporateRouteStopExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateRouteStop ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateRouteStop.CorporateRouteStop -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateRouteStop.CorporateRouteStop] -> m ())
createMany = traverse_ create

findByRouteId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute -> m [Domain.Types.CorporateRouteStop.CorporateRouteStop])
findByRouteId routeId = do findAllWithKV [Se.Is Beam.routeId $ Se.Eq (Kernel.Types.Id.getId routeId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateRouteStop.CorporateRouteStop -> m (Maybe Domain.Types.CorporateRouteStop.CorporateRouteStop))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
