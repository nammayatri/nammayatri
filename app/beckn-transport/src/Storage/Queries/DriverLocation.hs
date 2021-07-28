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
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverLocation as Storage
import qualified Types.Storage.Person as SP

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverLocationT))
getDbTable =
  DB.driverLocation . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.DriverLocation -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.DriverLocation -> DB.SqlDB ()
create location = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression location)

findById ::
  DBFlow m r =>
  Id SP.Person ->
  m (Maybe Storage.DriverLocation)
findById drLocId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.DriverLocation {..} = driverId ==. B.val_ drLocId

updateGpsCoord :: Id SP.Person -> UTCTime -> LatLong -> DB.SqlDB ()
updateGpsCoord drLocationId now (LatLong lat' long') = do
  locTable <- getDbTable
  DB.update' locTable (setClause lat' long' now) (predicate drLocationId)
  where
    setClause mLat mLong now' Storage.DriverLocation {..} =
      let point' = B.customExpr_ $ "public.ST_SetSRID(ST_Point(" <> show mLong <> ", " <> show mLat <> ")::geography, 4326)"
       in mconcat
            [ lat <-. B.val_ (Just mLat),
              long <-. B.val_ (Just mLong),
              updatedAt <-. B.val_ now',
              point <-. point'
            ]
    predicate drLocId Storage.DriverLocation {..} = driverId B.==. B.val_ drLocId

updateLocationRec :: DBFlow m r => Id SP.Person -> Storage.DriverLocation -> m ()
updateLocationRec drLocationId drLocation = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause drLocation now) (predicate drLocationId)
  where
    setClause drLoc n Storage.DriverLocation {..} =
      mconcat
        [ lat <-. B.val_ (Storage.lat drLoc),
          long <-. B.val_ (Storage.long drLoc),
          updatedAt <-. B.val_ n
        ]
    predicate drLocId Storage.DriverLocation {..} = driverId ==. B.val_ drLocId
