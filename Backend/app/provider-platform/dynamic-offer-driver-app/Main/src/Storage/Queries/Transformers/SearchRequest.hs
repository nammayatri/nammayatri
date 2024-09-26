module Storage.Queries.Transformers.SearchRequest where

import qualified Domain.Types.Location as Location
import Kernel.Prelude
-- import Kernel.Types.Error
-- import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

getStops :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m [Location.Location]
getStops id = do
  stopsLocationMapping <- QLM.getLatestStopsByEntityId id
  mapM
    ( \stopLocationMapping ->
        QL.findById stopLocationMapping.locationId
          >>= fromMaybeM (StopsLocationNotFound stopLocationMapping.locationId.getId)
    )
    stopsLocationMapping
