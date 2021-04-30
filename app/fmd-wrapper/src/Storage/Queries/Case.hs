module Storage.Queries.Case where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Case as Storage
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT))
getDbTable =
  DB._case . DB.appDb <$> getSchemaName

create :: Storage.Case -> Flow ()
create Storage.Case {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})

findById :: Id Storage.Case -> Flow (Maybe Storage.Case)
findById caseId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = _id ==. B.val_ caseId

update :: Id Storage.Case -> Storage.Case -> Flow ()
update id case_@Storage.Case {..} = do
  dbTable <- getDbTable
  currTime <- getCurrentTime
  DB.update dbTable (setClause currTime case_) (predicate id)
  where
    predicate cid Storage.Case {..} = _id ==. B.val_ cid
    setClause now c Storage.Case {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _status <-. B.val_ (Storage._status c),
          _shortId <-. B.val_ (Storage._shortId c),
          _startTime <-. B.val_ (Storage._startTime c),
          _endTime <-. B.val_ (Storage._endTime c),
          _parentCaseId <-. B.val_ (Storage._parentCaseId c),
          _udf1 <-. B.val_ (Storage._udf1 c),
          _udf2 <-. B.val_ (Storage._udf2 c),
          _udf3 <-. B.val_ (Storage._udf3 c),
          _udf4 <-. B.val_ (Storage._udf4 c),
          _udf5 <-. B.val_ (Storage._udf5 c),
          _info <-. B.val_ (Storage._info c)
        ]
