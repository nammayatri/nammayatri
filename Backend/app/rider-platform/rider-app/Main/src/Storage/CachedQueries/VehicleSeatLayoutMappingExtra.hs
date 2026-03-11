{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.VehicleSeatLayoutMappingExtra where

import Domain.Types.VehicleSeatLayoutMapping
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.VehicleSeatLayoutMapping as Queries

fiveDaysTTL :: Int
fiveDaysTTL = 5 * 24 * 60 * 60 -- 432000 seconds

makeKey :: Text -> Text -> Text
makeKey vehicleNo gtfsId =
  "CachedQueries:VehicleSeatLayoutMapping:VehicleNo-" <> vehicleNo <> ":GtfsId-" <> gtfsId

findByVehicleNoAndGtfsIdCached ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  m (Maybe VehicleSeatLayoutMapping)
findByVehicleNoAndGtfsIdCached vehicleNo gtfsId =
  let key = makeKey vehicleNo gtfsId
   in Hedis.safeGet key >>= \case
        Just val -> return val
        Nothing -> do
          val <- Queries.findByVehicleNoAndGtfsId vehicleNo gtfsId
          whenJust val $ \v ->
            Hedis.setExp key v fiveDaysTTL
          return val

invalidateCache :: (CacheFlow m r) => Text -> Text -> m ()
invalidateCache vehicleNo gtfsId =
  Hedis.del (makeKey vehicleNo gtfsId)
