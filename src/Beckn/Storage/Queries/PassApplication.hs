module Beckn.Storage.Queries.PassApplication where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.DB as DB
import qualified Beckn.Types.Storage.PassApplication as Storage
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable ::
     B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.PassApplicationT)
dbTable = DB._passApplication DB.becknDb

create :: Storage.PassApplication -> L.Flow ()
create Storage.PassApplication {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.PassApplication {..}) >>=
  either DB.throwDBError pure

findPassApplicationById ::
     PassApplicationId -> L.Flow (T.DBResult (Maybe Storage.PassApplication))
findPassApplicationById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.PassApplication {..} = (_id ==. B.val_ id)
