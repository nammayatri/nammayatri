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
incrementTotalRides rcId@(Id rcId') = do
  mbStats <- findById rcId
  case mbStats of
    Nothing -> do
      -- Create entry with totalRides = 1 directly
      now <- getCurrentTime
      let rcStats =
            RCStats
              { rcId = rcId,
                totalRides = 1,
                updatedAt = now
              }
      QRCStats.create rcStats
      pure 1
    Just stats -> do
      -- Entry exists, increment it
      let currentRides = stats.totalRides
          newTotalRides = currentRides + 1
      now <- getCurrentTime
      updateOneWithKV
        [ Se.Set (\BeamRS.RCStatsT {BeamRS.totalRides = tr} -> tr) newTotalRides,
          Se.Set BeamRS.updatedAt now
        ]
        [Se.Is BeamRS.rcId (Se.Eq rcId')]
      pure newTotalRides
