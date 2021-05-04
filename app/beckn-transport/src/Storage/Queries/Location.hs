{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.Location where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Location as Storage
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LocationT))
getDbTable =
  DB._location . DB.transporterDb <$> getSchemaName

createFlow :: Storage.Location -> Flow ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Location -> DB.SqlDB ()
create location = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression location)

findLocationById ::
  Id Storage.Location -> Flow (Maybe Storage.Location)
findLocationById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Location {..} = _id ==. B.val_ id

updateLocationRec :: Id Storage.Location -> Storage.Location -> Flow ()
updateLocationRec locationId location = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause location now) (predicate locationId)
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
  DB.findAll dbTable identity (predicate (Id <$> fromIds) (Id <$> toIds))
  where
    predicate pFromIds pToIds Storage.Location {..} =
      B.in_ _id (B.val_ <$> pFromIds)
        ||. B.in_ _id (B.val_ <$> pToIds)

updateGpsCoord :: Id Storage.Location -> Double -> Double -> Flow ()
updateGpsCoord locationId lat long = do
  locTable <- getDbTable
  now <- getCurrentTime
  DB.update locTable (setClause lat long now) (predicate locationId)
  where
    setClause mLat mLong now Storage.Location {..} =
      let point = B.customExpr_ $ "public.ST_SetSRID(ST_Point(" <> show mLong <> ", " <> show mLat <> ")::geography, 4326)"
       in mconcat
            [ _lat <-. B.val_ (Just mLat),
              _long <-. B.val_ (Just mLong),
              _updatedAt <-. B.val_ now,
              _point <-. point
            ]
    predicate locId Storage.Location {..} = _id B.==. B.val_ locId
