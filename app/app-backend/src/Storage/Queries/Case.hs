module Storage.Queries.Case where

import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Storage
import           Beckn.Utils.Common
import           Data.Time
import           Database.Beam            ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam            as B
import qualified EulerHS.Language         as L
import           EulerHS.Prelude          hiding (id)
import qualified EulerHS.Types            as T
import qualified Storage.Queries          as DB
import           Types.App
import qualified Types.Storage.DB         as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.appDb

create :: Storage.Case -> L.Flow ()
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})
    >>= either DB.throwDBError pure

findAllByType :: Integer -> Integer -> Storage.CaseType -> Storage.CaseStatus -> L.Flow [Storage.Case]
findAllByType limit offset caseType caseStatus =
  DB.findAllWithLimitOffsetWhere dbTable (predicate caseType caseStatus) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate caseType caseStatus Storage.Case {..} =
      ( _type ==. (B.val_ caseType)
          &&. _status ==. (B.val_ caseStatus)
      )

findById :: CaseId -> L.Flow Storage.Case
findById caseId =
  DB.findOneWithErr dbTable (predicate caseId)
  where
    predicate caseId Storage.Case {..} = _id ==. (B.val_ caseId)

findAllByPerson :: Text -> L.Flow [Storage.Case]
findAllByPerson perId =
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Case {..} = _requestor ==. B.val_ (Just perId)

updateStatus :: CaseId -> Storage.CaseStatus  -> L.Flow ()
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status currTime Storage.Case {..} =
      mconcat
         [ _status <-. B.val_ status,
            _updatedAt <-. B.val_ currTime
         ]

    predicate id Storage.Case {..} = _id ==. B.val_ id


updateStatusAndUdfs :: CaseId -> Storage.CaseStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> L.Flow ()
updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5 = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status udf1 udf2 udf3 udf4 udf5 currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status udf1 udf2 udf3 udf4 udf5 currTime Storage.Case {..} =
      mconcat
         [ _status <-. B.val_ status
         , _updatedAt <-. B.val_ currTime
         , _udf1 <-. B.val_ udf1
         , _udf2 <-. B.val_ udf2
         , _udf3 <-. B.val_ udf3
         , _udf4 <-. B.val_ udf4
         , _udf5 <-. B.val_ udf5
         ]

    predicate id Storage.Case {..} = _id ==. B.val_ id

findAllWithLimitOffsetWhere :: [Text] -> [Text] -> [Storage.CaseType] -> [Storage.CaseStatus] -> [Text] -> Maybe Int -> Maybe Int -> L.Flow [Storage.Case]
findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset =
    DB.findAllWithLimitOffsetWhere
      dbTable
      (predicate  fromLocationIds toLocationIds types statuses udf1s)
      limit
      offset
      orderByDesc
      >>= either DB.throwDBError pure
    where
      limit = (toInteger $ fromMaybe 100 mlimit)
      offset = (toInteger $ fromMaybe 0 moffset)
      orderByDesc Storage.Case {..} = B.desc_ _createdAt
      predicate fromLocationIds toLocationIds types statuses udf1s Storage.Case {..} =
          foldl
            (&&.)
            (B.val_ True)
            [ _fromLocationId `B.in_` ((B.val_) <$> fromLocationIds) ||. complementVal fromLocationIds,
             _toLocationId `B.in_` ((B.val_) <$> toLocationIds) ||. complementVal toLocationIds,
              _status `B.in_` ((B.val_ ) <$> statuses) ||. complementVal statuses,
              _type `B.in_` ((B.val_ ) <$> types) ||. complementVal types,
              _udf1 `B.in_` ((B.val_ . Just ) <$> udf1s) ||. complementVal udf1s
            ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False
