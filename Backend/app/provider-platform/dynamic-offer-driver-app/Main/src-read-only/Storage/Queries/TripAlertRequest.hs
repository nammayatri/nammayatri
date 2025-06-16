{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TripAlertRequest (module Storage.Queries.TripAlertRequest, module ReExport) where

import qualified Domain.Types.TripAlertRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TripAlertRequest as Beam
import Storage.Queries.TripAlertRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TripAlertRequest.TripAlertRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TripAlertRequest.TripAlertRequest] -> m ())
createMany = traverse_ create

updateIsViolated :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.TripAlertRequest.TripAlertRequest -> m ())
updateIsViolated isViolated id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.isViolated isViolated, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TripAlertRequest.TripAlertRequest -> m (Maybe Domain.Types.TripAlertRequest.TripAlertRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TripAlertRequest.TripAlertRequest -> m ())
updateByPrimaryKey (Domain.Types.TripAlertRequest.TripAlertRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.alertRequestId (Kernel.Types.Id.getId alertRequestId),
      Se.Set Beam.alertRequestType alertRequestType,
      Se.Set Beam.alertStatus alertStatus,
      Se.Set Beam.conductorFleetBadgeId (Kernel.Types.Id.getId <$> conductorFleetBadgeId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.fleetBadgeId (Kernel.Types.Id.getId <$> driverFleetBadgeId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.isViolated isViolated,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.tripTransactionId (Kernel.Types.Id.getId tripTransactionId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
