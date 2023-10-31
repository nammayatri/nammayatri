module Domain.Action.UI.City where

import IssueManagement.Common.Dashboard.Issue (CacheFlow)
import IssueManagement.Tools.UtilsTH (runInReplica)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Common (EsqDBFlow)
import Storage.Queries.Geometry (findGeometriesContainingGps)

data GetCityReq = GetCityReq
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype GetCityResp = GetCityResp
  { city :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getCity :: (CacheFlow m r, EsqDBFlow m r) => GetCityReq -> m GetCityResp
getCity req = do
  let lat = req.lat
      lon = req.lon
  let latlng = LatLong {lat = lat, lon = lon}
  geometry <-
    runInReplica $
      findGeometriesContainingGps latlng >>= \case
        [] -> do
          pure Nothing
        (g : _) -> pure $ Just g
  let city = (.city) <$> geometry
  pure $ GetCityResp {city = show <$> city}
