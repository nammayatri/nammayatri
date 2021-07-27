{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.Location where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (UTCTime)
import Database.Beam ((<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.SearchReqLocation as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.SearchReqLocationT))
getDbTable =
  DB.searchReqLocation . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.SearchReqLocation -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.SearchReqLocation -> DB.SqlDB ()
create location = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression location)

findLocationById ::
  DBFlow m r =>
  Id Storage.SearchReqLocation ->
  m (Maybe Storage.SearchReqLocation)
findLocationById locId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.SearchReqLocation {..} = id ==. B.val_ locId

updateLocationRec :: DBFlow m r => Id Storage.SearchReqLocation -> Storage.SearchReqLocation -> m ()
updateLocationRec locationId location = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause location now) (predicate locationId)
  where
    setClause loc n Storage.SearchReqLocation {..} =
      mconcat
        [ locationType <-. B.val_ (Storage.locationType loc),
          lat <-. B.val_ (Storage.lat loc),
          long <-. B.val_ (Storage.long loc),
          ward <-. B.val_ (Storage.ward loc),
          district <-. B.val_ (Storage.district loc),
          city <-. B.val_ (Storage.city loc),
          state <-. B.val_ (Storage.state loc),
          country <-. B.val_ (Storage.country loc),
          pincode <-. B.val_ (Storage.pincode loc),
          address <-. B.val_ (Storage.address loc),
          bound <-. B.val_ (Storage.bound loc),
          updatedAt <-. B.val_ n
        ]
    predicate locId Storage.SearchReqLocation {..} = id ==. B.val_ locId

findAllByLocIds :: DBFlow m r => [Id Storage.SearchReqLocation] -> [Id Storage.SearchReqLocation] -> m [Storage.SearchReqLocation]
findAllByLocIds fromIds toIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate fromIds toIds)
  where
    predicate pFromIds pToIds Storage.SearchReqLocation {..} =
      B.in_ id (B.val_ <$> pFromIds)
        ||. B.in_ id (B.val_ <$> pToIds)

updateGpsCoord :: Id Storage.SearchReqLocation -> UTCTime -> LatLong -> DB.SqlDB ()
updateGpsCoord locationId now (LatLong lat' long') = do
  locTable <- getDbTable
  DB.update' locTable (setClause lat' long' now) (predicate locationId)
  where
    setClause mLat mLong now' Storage.SearchReqLocation {..} =
      let point' = B.customExpr_ $ "public.ST_SetSRID(ST_Point(" <> show mLong <> ", " <> show mLat <> ")::geography, 4326)"
       in mconcat
            [ lat <-. B.val_ (Just mLat),
              long <-. B.val_ (Just mLong),
              updatedAt <-. B.val_ now',
              point <-. point'
            ]
    predicate locId Storage.SearchReqLocation {..} = id B.==. B.val_ locId
