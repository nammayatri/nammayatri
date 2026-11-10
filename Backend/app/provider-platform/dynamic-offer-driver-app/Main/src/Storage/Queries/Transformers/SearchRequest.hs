module Storage.Queries.Transformers.SearchRequest where

import Domain.Types.Location (Location (..))
import qualified Domain.Types.Location as Location
import qualified Domain.Types.LocationMapping as Fields
import Kernel.Prelude
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

getLocationId :: Location -> Text
getLocationId Location {id = locId} = ID.getId locId

getStops :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> Maybe Bool -> m [Location.Location]
getStops id hasStops = do
  if hasStops == Just True
    then do
      stopsLocationMapping <- QLM.getLatestStopsByEntityId id
      mapM
        ( \stopLocationMapping ->
            QL.findById stopLocationMapping.locationId
              >>= fromMaybeM (StopsLocationNotFound stopLocationMapping.locationId.getId)
        )
        stopsLocationMapping
    else return []
