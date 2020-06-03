module Storage.Queries.Quotation where

import Beckn.Types.Common
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Quotation as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.QuotationT)
dbTable = DB._quotation DB.transporterDb

create :: Storage.Quotation -> L.Flow ()
create Storage.Quotation {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Quotation {..})
    >>= either DB.throwDBError pure

findQuotationById ::
  QuotationId -> L.Flow (Maybe Storage.Quotation)
findQuotationById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Quotation {..} = (_id ==. B.val_ id)

listQuotations :: Maybe Int -> Maybe Int -> [Storage.Status] -> L.Flow [Storage.Quotation]
listQuotations mlimit moffset status =
  DB.findAllWithLimitOffsetWhere dbTable (predicate status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    orderByDesc Storage.Quotation {..} = B.desc_ _createdAt
    predicate status Storage.Quotation {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status
        ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False

update ::
  QuotationId ->
  Storage.Status ->
  L.Flow (T.DBResult ())
update id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.Quotation {..} = _id ==. B.val_ id
    setClause status currTime Storage.Quotation {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]
