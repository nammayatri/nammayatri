{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleTripExtra where

import qualified Database.Beam as B
import qualified Domain.Types.VehicleTrip
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleTrip as Beam
import Storage.Queries.OrphanInstances.VehicleTrip

-- Extra code goes here --

newtype TripDB f = TripDB {trip :: f (B.TableEntity Beam.VehicleTripT)}
  deriving (Generic, B.Database be)

tripDB :: B.DatabaseSettings be TripDB
tripDB = B.defaultDbSettings `B.withDbModification` B.dbModification {trip = Beam.vehicleTripTable}

-- | Raw SQL on purpose: vehicle_trip is out of KV (ddl 1576), so this hits the same rows the KV-mode
-- helpers do, and `SET col = COALESCE(col, 0) + 1` never loses a concurrent increment.
incrementMissedPickups :: (MonadFlow m, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m ()
incrementMissedPickups tripId = do
  now <- getCurrentTime
  dbConf <- getMasterBeamConfig
  void . L.runDB dbConf . L.updateRows $
    B.update
      (trip tripDB)
      (\r -> (Beam.missedPickups r B.<-. B.just_ (B.coalesce_ [B.current_ (Beam.missedPickups r)] (B.val_ 0) + 1)) <> (Beam.updatedAt r B.<-. B.val_ now))
      (\r -> Beam.id r B.==. B.val_ (Kernel.Types.Id.getId tripId))
