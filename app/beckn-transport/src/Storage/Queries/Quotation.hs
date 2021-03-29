module Storage.Queries.Quotation where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Quotation as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.QuotationT))
getDbTable =
  DB._quotation . DB.transporterDb <$> getSchemaName

create :: Storage.Quotation -> Flow ()
create Storage.Quotation {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Quotation {..})
    >>= either throwDBError pure

findQuotationById ::
  Id Storage.Quotation -> Flow (Maybe Storage.Quotation)
findQuotationById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
  where
    predicate Storage.Quotation {..} = _id ==. B.val_ id

listQuotations :: Maybe Int -> Maybe Int -> [Storage.Status] -> Flow [Storage.Quotation]
listQuotations mlimit moffset status = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= either throwDBError pure
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Quotation {..} = B.desc_ _createdAt
    predicate Storage.Quotation {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  Id Storage.Quotation ->
  Storage.Status ->
  Flow (T.DBResult ())
update id status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate qid Storage.Quotation {..} = _id ==. B.val_ qid
    setClause scStatus currTime Storage.Quotation {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]
