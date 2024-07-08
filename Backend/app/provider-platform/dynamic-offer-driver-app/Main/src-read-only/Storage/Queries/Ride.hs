{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Ride (module Storage.Queries.Ride, module ReExport) where

import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Ride as Beam
import Storage.Queries.RideExtra as ReExport
import Storage.Queries.Transformers.Ride

updateCancellationFeeIfCancelledField :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateCancellationFeeIfCancelledField cancellationFeeIfCancelled id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cancellationFeeIfCancelled cancellationFeeIfCancelled, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePreviousRideTripEndPosAndTime ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Maps.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updatePreviousRideTripEndPosAndTime previousRideTripEndPos previousRideTripEndTime id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.previousRideTripEndLat (Kernel.Prelude.fmap (.lat) previousRideTripEndPos),
      Se.Set Beam.previousRideTripEndLon (Kernel.Prelude.fmap (.lon) previousRideTripEndPos),
      Se.Set Beam.previousRideTripEndTime previousRideTripEndTime,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
