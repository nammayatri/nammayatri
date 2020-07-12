module Storage.Queries.Location where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Location as Storage
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.transporterDb

create :: Storage.Location -> Flow ()
create Storage.Location {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Location {..})
    >>= either DB.throwDBError pure

findLocationById ::
  LocationId -> Flow Storage.Location
findLocationById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_DATA"
  where
    predicate Storage.Location {..} = _id ==. B.val_ id

updateLocationRec :: LocationId -> Storage.Location -> Flow ()
updateLocationRec locationId location = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause location now) (predicate locationId)
    >>= either DB.throwDBError pure
  where
    setClause location n Storage.Location {..} =
      mconcat
        [ _locationType <-. B.val_ (Storage._locationType location),
          _lat <-. B.val_ (Storage._lat location),
          _long <-. B.val_ (Storage._long location),
          _ward <-. B.val_ (Storage._ward location),
          _district <-. B.val_ (Storage._district location),
          _city <-. B.val_ (Storage._city location),
          _state <-. B.val_ (Storage._state location),
          _country <-. B.val_ (Storage._country location),
          _pincode <-. B.val_ (Storage._pincode location),
          _address <-. B.val_ (Storage._address location),
          _bound <-. B.val_ (Storage._bound location),
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Location {..} = _id ==. B.val_ id

findAllByLocIds :: [Text] -> [Text] -> Flow [Storage.Location]
findAllByLocIds fromIds toIds =
  DB.findAllOrErr dbTable (pred (LocationId <$> fromIds) (LocationId <$> toIds))
  where
    pred fromIds toIds Storage.Location {..} =
      B.in_ _id (B.val_ <$> fromIds)
        ||. B.in_ _id (B.val_ <$> toIds)
