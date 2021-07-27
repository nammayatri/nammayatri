{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import Beckn.Types.Schema
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverLocation as Storage
import Data.Time (UTCTime)

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
  Id Storage.DriverLocation ->
  m (Maybe Storage.DriverLocation)
findById drLocId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.DriverLocation {..} = id ==. B.val_ drLocId

updateGpsCoord :: Id Storage.DriverLocation -> UTCTime -> LatLong -> DB.SqlDB ()
updateGpsCoord locationId now (LatLong lat' long') = do
  locTable <- getDbTable
  DB.update' locTable (setClause lat' long' now) (predicate locationId)
  where
    setClause mLat mLong now' Storage.DriverLocation {..} =
      let point' = B.customExpr_ $ "public.ST_SetSRID(ST_Point(" <> show mLong <> ", " <> show mLat <> ")::geography, 4326)"
       in mconcat
            [ lat <-. B.val_ (Just mLat),
              long <-. B.val_ (Just mLong),
              updatedAt <-. B.val_ now',
              point <-. point'
            ]
    predicate locId Storage.DriverLocation {..} = id B.==. B.val_ locId

updateLocationRec :: DBFlow m r => Id Storage.DriverLocation -> Storage.DriverLocation -> m ()
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
    predicate drLocId Storage.DriverLocation {..} = id ==. B.val_ drLocId