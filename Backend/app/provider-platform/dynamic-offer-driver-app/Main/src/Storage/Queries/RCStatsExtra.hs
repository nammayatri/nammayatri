module Storage.Queries.RCStatsExtra where

import Domain.Types.RCStats as Domain
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RCStats as BeamRS
import qualified Storage.Queries.RCStats as QRCStats

-- Extra code goes here --

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DRC.VehicleRegistrationCertificate -> m (Maybe RCStats)
findById = QRCStats.findById

findTotalRides :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DRC.VehicleRegistrationCertificate -> m Int
findTotalRides rcId = maybe (pure 0) (pure . Domain.totalRides) =<< QRCStats.findById rcId

incrementTotalRides :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DRC.VehicleRegistrationCertificate -> m Int
incrementTotalRides (Id rcId') = do
  now <- getCurrentTime
  newTotalRides <-
    findTotalRides (Id rcId') >>= \rides -> do
      updateOneWithKV
        [ Se.Set (\BeamRS.RCStatsT {..} -> totalRides) (rides + 1),
          Se.Set BeamRS.updatedAt now
        ]
        [Se.Is BeamRS.rcId (Se.Eq rcId')]
      pure (rides + 1)
  pure newTotalRides

createInitialRCStats :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DRC.VehicleRegistrationCertificate -> m ()
createInitialRCStats rcId = do
  now <- getCurrentTime
  let rcStats =
        RCStats
          { rcId = rcId,
            totalRides = 0,
            updatedAt = now
          }
  QRCStats.create rcStats
