module Storage.Queries.Leads where

import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Leads as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LeadsT)
dbTable = DB._leads DB.transporterDb

create :: Storage.Leads -> L.Flow ()
create Storage.Leads {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Leads {..})
    >>= either DB.throwDBError pure

findLeadsById ::
  LeadsId -> L.Flow (Maybe Storage.Leads)
findLeadsById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Leads {..} = (_id ==. B.val_ id)
