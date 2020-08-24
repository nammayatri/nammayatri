module Storage.Queries.Location where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Storage
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.), (||.))
import qualified Database.Beam as B
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

findLocationById ::
  LocationId -> Flow Storage.Location
findLocationById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_DATA"
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
