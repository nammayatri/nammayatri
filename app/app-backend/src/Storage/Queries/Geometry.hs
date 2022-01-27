module Storage.Queries.Geometry where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.MapSearch (LatLong)
import Domain.Types.Geometry
import Storage.Tabular.Geometry

findGeometriesContaining :: EsqDBFlow m r => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions =
  runTransaction . findAll' $ do
    geometry <- from $ table @GeometryT
    where_ $
      geometry ^. GeometryRegion `in_` valList regions
        &&. containsPoint (gps.lon, gps.lat)
    return geometry

someGeometriesContain :: EsqDBFlow m r => LatLong -> [Text] -> m Bool
someGeometriesContain gps regions = do
  geometries <- findGeometriesContaining gps regions
  pure $ not $ null geometries
