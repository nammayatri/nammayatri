{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteDetailsExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.OrphanInstances.RouteDetails

-- Extra code goes here --
-- Update tracking status with leg start and end time
updateTrackingStatusWithTime ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Lib.JourneyModule.State.Types.TrackingStatus -> Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails -> m ())
updateTrackingStatusWithTime trackingStatus id = do
  _now <- getCurrentTime
  updateOneWithKV
    ([Se.Set Beam.trackingStatus trackingStatus, Se.Set Beam.updatedAt _now, Se.Set Beam.trackingStatusLastUpdatedAt (Just _now)])
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTrackingStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Lib.JourneyModule.State.Types.TrackingStatus -> Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails -> m ())
updateTrackingStatus trackingStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.trackingStatus trackingStatus, Se.Set Beam.updatedAt _now, Se.Set Beam.trackingStatusLastUpdatedAt (Just _now)] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
