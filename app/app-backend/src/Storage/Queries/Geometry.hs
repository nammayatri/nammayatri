module Storage.Queries.Geometry where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.MapSearch (LatLong)
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((&&.))
import qualified Database.Beam as B
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import Types.Storage.Geometry as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.GeometryT))
getDbTable =
  DB.geometry . DB.appDb <$> getSchemaName

containsPoint :: (Monoid a, IsString a) => a -> a
containsPoint point = "st_contains(geom, ST_GeomFromText(" <> point <> "))"

containsPoint_ ::
  B.QGenExpr ctxt Postgres s Text ->
  B.QGenExpr ctxt Postgres s Bool
containsPoint_ = B.customExpr_ containsPoint

containsPredicate ::
  LatLong ->
  GeometryT (B.QExpr Postgres B.QBaseScope) ->
  B.QGenExpr B.QValueContext Postgres B.QBaseScope Bool
containsPredicate gps _ = containsPoint_ (B.val_ point)
  where
    point = "POINT (" <> show gps.lon <> " " <> show gps.lat <> ")"

findGeometriesContaining :: DBFlow m r => LatLong -> [Text] -> m [Storage.Geometry]
findGeometriesContaining gps regions = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate geometry@Geometry {..} =
      B.in_ region (B.val_ <$> regions) &&. containsPredicate gps geometry

someGeometriesContain :: DBFlow m r => LatLong -> [Text] -> m Bool
someGeometriesContain gps regions = do
  geometries <- findGeometriesContaining gps regions
  pure $ not $ null geometries
