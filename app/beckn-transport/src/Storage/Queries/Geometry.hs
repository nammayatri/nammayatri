module Storage.Queries.Geometry where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.MapSearch (LatLong)
import Domain.Types.Geometry
import Storage.Tabular.Geometry

findGeometriesContaining :: Transactionable m => LatLong -> [Text] -> m [Geometry]
findGeometriesContaining gps regions =
  Esq.findAll $ do
    geometry <- from $ table @GeometryT
    where_ $
      geometry ^. GeometryRegion `in_` valList regions
        &&. containsPoint (gps.lon, gps.lat)
    return geometry

someGeometriesContain :: Transactionable m => LatLong -> [Text] -> m Bool
someGeometriesContain gps regions = do
  geometries <- findGeometriesContaining gps regions
  pure $ not $ null geometries
