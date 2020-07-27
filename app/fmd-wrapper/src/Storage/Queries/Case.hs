module Storage.Queries.Case where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Storage
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.appDb

create :: Storage.Case -> Flow (T.DBResult ())
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})

findById :: CaseId -> Flow (T.DBResult (Maybe Storage.Case))
findById caseId =
  DB.findOne dbTable (predicate caseId)
  where
    predicate caseId Storage.Case {..} = _id ==. B.val_ caseId
