module Storage.Queries.Geometry where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import Beckn.Types.Storage.Geometry as Storage
import Beckn.Utils.Common
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Types.Common
import qualified Types.Storage.DB as DB

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
  GPS ->
  GeometryT (B.QExpr Postgres B.QBaseScope) ->
  B.QGenExpr B.QValueContext Postgres B.QBaseScope Bool
containsPredicate gps _ = containsPoint_ (B.val_ point)
  where
    point = "POINT (" <> gps.lon <> " " <> gps.lat <> ")"

findGeometriesContaining :: HasFlowDBEnv m r => GPS -> Text -> m [Storage.Geometry]
findGeometriesContaining gps region_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate geometry@Geometry {..} =
      region ==. B.val_ region_ &&. containsPredicate gps geometry

someGeometriesContain :: HasFlowDBEnv m r => GPS -> Text -> m Bool
someGeometriesContain gps region = do
  geometries <- findGeometriesContaining gps region
  pure $ not $ null geometries
