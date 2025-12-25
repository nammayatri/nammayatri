{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteDetailsExtra where

import qualified Domain.Types.RouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.JourneyModule.State.Types
import qualified Sequelize as Se
import qualified Storage.Beam.RouteDetails as Beam
import Storage.Queries.OrphanInstances.RouteDetails

-- Update tracking status with leg start and end time
updateTrackingStatusWithTime ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Lib.JourneyModule.State.Types.TrackingStatus -> Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails -> UTCTime -> m ())
updateTrackingStatusWithTime trackingStatus id trackingStatusUpdateTime = do
  _now <- getCurrentTime
  let timeUpdates = case trackingStatus of
        Just Lib.JourneyModule.State.Types.Ongoing -> [Se.Set Beam.legStartTime (Just _now)]
        Just Lib.JourneyModule.State.Types.Finished -> [Se.Set Beam.legEndTime (Just _now)]
        _ -> []
  updateOneWithKV
    (timeUpdates <> [Se.Set Beam.trackingStatus trackingStatus, Se.Set Beam.updatedAt _now, Se.Set Beam.trackingStatusLastUpdatedAt (Just trackingStatusUpdateTime)])
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTrackingStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Lib.JourneyModule.State.Types.TrackingStatus -> Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails -> m ())
updateTrackingStatus trackingStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.trackingStatus trackingStatus, Se.Set Beam.updatedAt _now, Se.Set Beam.trackingStatusLastUpdatedAt (Just _now)] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
