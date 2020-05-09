module Storage.Queries.Driver where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Beckn.Storage.Queries            as DB
import           Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Types.Storage.Driver             as Storage
import           Beckn.Utils.Common
import           Data.Time
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverT)
dbTable = DB._driver DB.transporterDb

create :: Storage.Driver -> L.Flow ()
create Storage.Driver {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Driver {..}) >>=
  either DB.throwDBError pure

findDriverById ::
     DriverId -> L.Flow (Maybe Storage.Driver)
findDriverById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Driver {..} = (_id ==. B.val_ id)
