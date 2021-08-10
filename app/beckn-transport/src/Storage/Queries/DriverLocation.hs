{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import Beckn.Types.Schema
import Data.Time (UTCTime)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverLocation as Storage
import qualified Types.Storage.Person as SP

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverLocationT))
getDbTable =
  DB.driverLocation . DB.transporterDb <$> getSchemaName

create :: Id SP.Person -> LatLong -> UTCTime -> DB.SqlDB ()
create drLocId (LatLong lat long) updateTime = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression insertClause)
  where
    insertClause :: Storage.DriverLocationT (B.QExpr Postgres s)
    insertClause = do
      Storage.DriverLocation
        { driverId = B.val_ drLocId,
          lat = B.val_ lat,
          long = B.val_ long,
          point = mkPointExpr long lat,
          createdAt = B.val_ updateTime,
          updatedAt = B.val_ updateTime
        }

findById ::
  DBFlow m r =>
  Id SP.Person ->
  m (Maybe Storage.DriverLocation)
findById drLocId = DB.runSqlDB $ findById' drLocId

findById' ::
  Id SP.Person ->
  DB.SqlDB (Maybe Storage.DriverLocation)
findById' drLocId = do
  dbTable <- getDbTable
  DB.findOne' dbTable predicate
  where
    predicate Storage.DriverLocation {..} = driverId ==. B.val_ drLocId

upsertGpsCoord :: Id SP.Person -> LatLong -> UTCTime -> DB.SqlDB ()
upsertGpsCoord drLocationId latLong updateTime = do
  loc <- findById' drLocationId
  case loc of
    Nothing -> create drLocationId latLong updateTime
    Just _ -> updateGpsCoord drLocationId latLong updateTime

updateGpsCoord :: Id SP.Person -> LatLong -> UTCTime -> DB.SqlDB ()
updateGpsCoord drLocationId (LatLong lat' long') updateTime = do
  locTable <- getDbTable
  DB.update' locTable (setClause lat' long' updateTime) (predicate drLocationId)
  where
    setClause mLat mLong updTime Storage.DriverLocation {..} =
      mconcat
        [ lat <-. B.val_ mLat,
          long <-. B.val_ mLong,
          updatedAt <-. B.val_ updTime,
          point <-. mkPointExpr mLong mLat
        ]
    predicate drLocId Storage.DriverLocation {..} = driverId B.==. B.val_ drLocId

mkPointExpr ::
  ( B.IsCustomExprFn fn res,
    Semigroup fn,
    IsString fn,
    Show a1,
    Show a2
  ) =>
  a1 ->
  a2 ->
  res
mkPointExpr mLong mLat = B.customExpr_ $ "public.ST_SetSRID(ST_Point(" <> show mLong <> ", " <> show mLat <> ")::geography, 4326)"
