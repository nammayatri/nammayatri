module Storage.Queries.Case where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Case as Storage
import qualified Types.Storage.DB as DB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT))
getDbTable =
  DB._case . DB.appDb <$> getSchemaName

create :: DBFlow m r => Storage.Case -> m ()
create Storage.Case {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue Storage.Case {..})

findById :: DBFlow m r => Id Storage.Case -> m (Maybe Storage.Case)
findById caseId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = id ==. B.val_ caseId

update :: DBFlow m r => Id Storage.Case -> Storage.Case -> m ()
update cid case_ = do
  dbTable <- getDbTable
  currTime <- getCurrentTime
  DB.update dbTable (setClause currTime case_) (predicate cid)
  where
    predicate cid_ Storage.Case {..} = id ==. B.val_ cid_
    setClause now c Storage.Case {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          status <-. B.val_ (Storage.status c),
          shortId <-. B.val_ (Storage.shortId c),
          startTime <-. B.val_ (Storage.startTime c),
          endTime <-. B.val_ (Storage.endTime c),
          parentCaseId <-. B.val_ (Storage.parentCaseId c),
          udf1 <-. B.val_ (Storage.udf1 c),
          udf2 <-. B.val_ (Storage.udf2 c),
          udf3 <-. B.val_ (Storage.udf3 c),
          udf4 <-. B.val_ (Storage.udf4 c),
          udf5 <-. B.val_ (Storage.udf5 c),
          info <-. B.val_ (Storage.info c)
        ]
