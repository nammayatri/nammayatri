module Storage.Queries.Quotation where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Quotation as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.QuotationT))
getDbTable =
  DB.quotation . DB.transporterDb <$> getSchemaName

create :: Storage.Quotation -> Flow ()
create Storage.Quotation {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Quotation {..})

findQuotationById ::
  Id Storage.Quotation -> Flow (Maybe Storage.Quotation)
findQuotationById qid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quotation {..} = id ==. B.val_ qid

listQuotations :: Maybe Int -> Maybe Int -> [Storage.Status] -> Flow [Storage.Quotation]
listQuotations mlimit moffset status_ = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Quotation {..} = B.desc_ createdAt
    predicate Storage.Quotation {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ status `B.in_` (B.val_ <$> status_) ||. complementVal status_
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  Id Storage.Quotation ->
  Storage.Status ->
  Flow ()
update qid status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate qid)
  where
    predicate qid_ Storage.Quotation {..} = id ==. B.val_ qid_
    setClause scStatus currTime Storage.Quotation {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]
