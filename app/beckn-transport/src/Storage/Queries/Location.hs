module Storage.Queries.Location where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Storage
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
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
      uuid <- L.generateGUID
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
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause lat long now) (predicate locationId)
    >>= either DB.throwDBError pure
  where
    setClause lLat lLong n Storage.Location {..} =
      mconcat
        [ _lat <-. B.val_ (Just lLat),
          _long <-. B.val_ (Just lLong),
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Location {..} = _id ==. B.val_ id
