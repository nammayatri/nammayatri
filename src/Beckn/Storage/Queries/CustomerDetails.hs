module Beckn.Storage.Queries.CustomerDetails where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.CustomerDetails as Storage
import qualified Beckn.Types.Storage.DB as DB
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable ::
     B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.CustomerDetailsT)
dbTable = DB._customerDetails DB.becknDb

create :: Storage.CustomerDetails -> L.Flow ()
create Storage.CustomerDetails {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.CustomerDetails {..}) >>=
  either DB.throwDBError pure

findCustomerDetailsById ::
     CustomerDetailsId -> L.Flow (T.DBResult (Maybe Storage.CustomerDetails))
findCustomerDetailsById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.CustomerDetails {..} = (_id ==. B.val_ id)
