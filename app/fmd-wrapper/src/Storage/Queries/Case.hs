module Storage.Queries.Case where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Storage
import Beckn.Utils.Common (checkDBError)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.appDb

create :: Storage.Case -> Flow ()
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})
    >>= checkDBError

findById :: CaseId -> Flow (Maybe Storage.Case)
findById caseId =
  DB.findOne dbTable (predicate caseId)
    >>= checkDBError
  where
    predicate caseId Storage.Case {..} = _id ==. B.val_ caseId

update :: CaseId -> Storage.Case -> Flow ()
update id case_@Storage.Case {..} = do
  currTime <- getCurrentTimeUTC
  DB.update dbTable (setClause currTime case_) (predicate id)
    >>= checkDBError
  where
    predicate id Storage.Case {..} = _id ==. B.val_ id
    setClause now case_ Storage.Case {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _status <-. B.val_ (Storage._status case_),
          _shortId <-. B.val_ (Storage._shortId case_),
          _status <-. B.val_ (Storage._status case_),
          _startTime <-. B.val_ (Storage._startTime case_),
          _endTime <-. B.val_ (Storage._endTime case_),
          _parentCaseId <-. B.val_ (Storage._parentCaseId case_),
          _udf1 <-. B.val_ (Storage._udf1 case_),
          _udf2 <-. B.val_ (Storage._udf2 case_),
          _udf3 <-. B.val_ (Storage._udf3 case_),
          _udf4 <-. B.val_ (Storage._udf4 case_),
          _udf5 <-. B.val_ (Storage._udf5 case_),
          _info <-. B.val_ (Storage._info case_)
        ]
