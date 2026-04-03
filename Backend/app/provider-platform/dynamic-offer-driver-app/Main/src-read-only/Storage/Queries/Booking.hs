{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.Booking (module Storage.Queries.Booking, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.BookingExtra as ReExport
import Storage.Queries.Transformers.Booking
import qualified Domain.Types.Booking
import qualified Storage.Beam.Booking as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Sequelize as Se



updateEstimatedDistanceAndFare :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                  (Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m ())
updateEstimatedDistanceAndFare estimatedFare estimatedDistance id = do {_now <- getCurrentTime;
                                                                        updateOneWithKV [Se.Set Beam.estimatedFare estimatedFare, Se.Set Beam.estimatedDistance estimatedDistance, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}



