{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Geometry where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import Beckn.Types.Storage.Geometry as Storage
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import Database.Beam.Postgres
import EulerHS.Prelude hiding (id)
import Types.Common
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.GeometryT))
getDbTable =
  DB._geometry . DB.appDb <$> getSchemaName

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
    point = "POINT (" <> gps ^. #lon <> " " <> gps ^. #lat <> ")"

findGeometriesContaining :: GPS -> Text -> Flow [Storage.Geometry]
findGeometriesContaining gps region = do
  do
    dbTable <- getDbTable
    DB.findAllOrErr dbTable predicate
  where
    predicate geometry@Geometry {..} =
      _region ==. B.val_ region &&. containsPredicate gps geometry

someGeometriesContain :: GPS -> Text -> Flow Bool
someGeometriesContain gps region = do
  geometries <- findGeometriesContaining gps region
  pure $ not $ null geometries
