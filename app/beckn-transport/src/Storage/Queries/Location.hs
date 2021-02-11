{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Storage.Queries.Location where

import App.Types
import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config (DBConfig (..))
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Storage
import Beckn.Utils.Common
import Data.Pool (withResource)
import Data.Time (UTCTime)
import Database.Beam ((<-.), (==.), (||.))
import qualified Database.Beam as B
import Database.PostgreSQL.Simple (execute)
import Database.PostgreSQL.Simple.Internal (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import EulerHS.Language (generateGUID, getSqlDBConnection, runIO)
import EulerHS.Prelude hiding (id)
import EulerHS.Types (SqlConn (PostgresPool), mkPostgresPoolConfig)
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LocationT))
getDbTable =
  DB._location . DB.transporterDb <$> getSchemaName

create :: Storage.Location -> Flow ()
create Storage.Location {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Location {..})
    >>= either DB.throwDBError pure

createDriverLoc :: Flow Storage.Location
createDriverLoc = do
  dbTable <- getDbTable
  location <- mkLoc
  DB.createOne dbTable (Storage.insertExpression location)
    >>= either DB.throwDBError (\_ -> pure location)
  where
    mkLoc = do
      uuid <- generateGUID
      now <- getCurrTime
      let n = Nothing
      return $
        Storage.Location
          { _id = LocationId uuid,
            _locationType = Storage.POINT,
            _lat = n,
            _long = n,
            _ward = n,
            _district = n,
            _city = n,
            _state = n,
            _country = n,
            _pincode = n,
            _address = n,
            _bound = n,
            _createdAt = now,
            _updatedAt = now
          }

findLocationById ::
  LocationId -> Flow (Maybe Storage.Location)
findLocationById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Location {..} = _id ==. B.val_ id

updateLocationRec :: LocationId -> Storage.Location -> Flow ()
updateLocationRec locationId location = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause location now) (predicate locationId)
    >>= either DB.throwDBError pure
  where
    setClause loc n Storage.Location {..} =
      mconcat
        [ _locationType <-. B.val_ (Storage._locationType loc),
          _lat <-. B.val_ (Storage._lat loc),
          _long <-. B.val_ (Storage._long loc),
          _ward <-. B.val_ (Storage._ward loc),
          _district <-. B.val_ (Storage._district loc),
          _city <-. B.val_ (Storage._city loc),
          _state <-. B.val_ (Storage._state loc),
          _country <-. B.val_ (Storage._country loc),
          _pincode <-. B.val_ (Storage._pincode loc),
          _address <-. B.val_ (Storage._address loc),
          _bound <-. B.val_ (Storage._bound loc),
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Location {..} = _id ==. B.val_ id

findAllByLocIds :: [Text] -> [Text] -> Flow [Storage.Location]
findAllByLocIds fromIds toIds = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable (predicate (LocationId <$> fromIds) (LocationId <$> toIds))
  where
    predicate pFromIds pToIds Storage.Location {..} =
      B.in_ _id (B.val_ <$> pFromIds)
        ||. B.in_ _id (B.val_ <$> pToIds)

updateGpsCoord :: LocationId -> Double -> Double -> Flow ()
updateGpsCoord locationId lat long = do
  let id = _getLocationId locationId
  DBConfig {..} <- asks dbCfg
  now <- getCurrTime
  pool <-
    getSqlDBConnection (mkPostgresPoolConfig connTag pgConfig poolConfig)
      >>= either DB.throwDBError pure
      >>= \case
        PostgresPool _connTag pool -> pure pool
        _ -> throwError500 "NOT_POSTGRES_BACKEND"
  void $ runIO $ withResource pool (runRawQuery id now)
  where
    runRawQuery :: Text -> UTCTime -> Connection -> IO Int64
    runRawQuery locId now conn = do
      let sqlQuery =
            [sql|
              UPDATE
                atlas_transporter.location
              SET
                point = public.ST_SetSRID(ST_Point(?, ?)::geography, 4326),
                long = ?,
                lat = ?,
                updated_at = ?
              WHERE
                id = ?
            |]
      execute
        conn
        sqlQuery
        (long, lat, long, lat, now, locId)
