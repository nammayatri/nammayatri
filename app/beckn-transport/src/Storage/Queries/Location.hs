{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.Location where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id, state)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Location as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LocationT))
getDbTable =
  DB.location . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Location -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Location -> DB.SqlDB ()
create location = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression location)

findLocationById ::
  DBFlow m r =>
  Id Storage.Location ->
  m (Maybe Storage.Location)
findLocationById locId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Location {..} = id ==. B.val_ locId

updateLocationRec :: DBFlow m r => Id Storage.Location -> Storage.Location -> m ()
updateLocationRec locationId location = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause location now) (predicate locationId)
  where
    setClause loc n Storage.Location {..} =
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
    predicate locId Storage.Location {..} = id ==. B.val_ locId

findAllByLocIds :: DBFlow m r => [Id Storage.Location] -> [Id Storage.Location] -> m [Storage.Location]
findAllByLocIds fromIds toIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate fromIds toIds)
  where
    predicate pFromIds pToIds Storage.Location {..} =
      B.in_ id (B.val_ <$> pFromIds)
        ||. B.in_ id (B.val_ <$> pToIds)

updateGpsCoord :: DBFlow m r => Id Storage.Location -> Double -> Double -> m ()
updateGpsCoord locationId lat_ long_ = do
  locTable <- getDbTable
  now <- getCurrentTime
  DB.update locTable (setClause lat_ long_ now) (predicate locationId)
  where
    setClause mLat mLong now Storage.Location {..} =
      let point_ = B.customExpr_ $ "public.ST_SetSRID(ST_Point(" <> show mLong <> ", " <> show mLat <> ")::geography, 4326)"
       in mconcat
            [ lat <-. B.val_ (Just mLat),
              long <-. B.val_ (Just mLong),
              updatedAt <-. B.val_ now,
              point <-. point_
            ]
    predicate locId Storage.Location {..} = id B.==. B.val_ locId
