{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateRouteStopAssignment (module Storage.Queries.CorporateRouteStopAssignment, module ReExport) where

import qualified Domain.Types.CorporateEmployee
import qualified Domain.Types.CorporateRouteStop
import qualified Domain.Types.CorporateRouteStopAssignment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateRouteStopAssignment as Beam
import Storage.Queries.CorporateRouteStopAssignmentExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateRouteStopAssignment ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment] -> m ())
createMany = traverse_ create

findByRouteStopId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateRouteStop.CorporateRouteStop -> m [Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment])
findByRouteStopId routeStopId = do findAllWithKV [Se.Is Beam.routeStopId $ Se.Eq (Kernel.Types.Id.getId routeStopId)]

findByEmployeeId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee -> m [Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment])
findByEmployeeId employeeId = do findAllWithKV [Se.Is Beam.employeeId $ Se.Eq (Kernel.Types.Id.getId employeeId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment -> m (Maybe Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
