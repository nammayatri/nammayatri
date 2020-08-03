{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Case where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Storage
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.appDb

create :: Storage.Case -> Flow (T.DBResult ())
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})

findAllByTypeAndStatuses ::
  PersonId ->
  Storage.CaseType ->
  [Storage.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  Flow (T.DBResult [Storage.Case])
findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset =
  let limit = fromMaybe 100 mlimit
      offset = fromMaybe 0 moffset
   in DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _type ==. B.val_ caseType,
          B.in_ _status (B.val_ <$> caseStatuses) ||. complementVal caseStatuses,
          _requestor ==. B.val_ (Just $ _getPersonId personId)
        ]

findById :: CaseId -> Flow (T.DBResult (Maybe Storage.Case))
findById caseId =
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = _id ==. B.val_ caseId

findByIdAndType :: CaseId -> Storage.CaseType -> Flow (T.DBResult (Maybe Storage.Case))
findByIdAndType caseId caseType =
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} =
      (_id ==. B.val_ caseId)
        &&. (_type ==. B.val_ caseType)

findIdByPerson :: Person.Person -> CaseId -> Flow (T.DBResult (Maybe Storage.Case))
findIdByPerson person caseId = do
  let personId = _getPersonId $ person ^. #_id
  DB.findOne dbTable (predicate personId)
  where
    predicate personId Storage.Case {..} =
      _id ==. B.val_ caseId &&. _requestor ==. B.val_ (Just personId)

findAllByIds :: [CaseId] -> Flow (T.DBResult [Storage.Case])
findAllByIds caseIds =
  DB.findAll dbTable predicate
  where
    predicate Storage.Case {..} = _id `B.in_` (B.val_ <$> caseIds)

findAllByPerson :: Text -> Flow (T.DBResult [Storage.Case])
findAllByPerson perId =
  DB.findAll dbTable predicate
  where
    predicate Storage.Case {..} = _requestor ==. B.val_ (Just perId)

findAllExpiredByStatus :: [Storage.CaseStatus] -> Maybe LocalTime -> Maybe LocalTime -> Flow (T.DBResult [Storage.Case])
findAllExpiredByStatus statuses maybeFrom maybeTo = do
  (now :: LocalTime) <- getCurrentTimeUTC
  DB.findAll dbTable (predicate now)
  where
    predicate now Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        ( [ _status `B.in_` (B.val_ <$> statuses),
            _validTill B.<=. B.val_ now
          ]
            <> maybe [] (\from -> [_createdAt B.>=. B.val_ from]) maybeFrom
            <> maybe [] (\to -> [_createdAt B.<=. B.val_ to]) maybeTo
        )

updateValidTill :: CaseId -> LocalTime -> Flow (T.DBResult ())
updateValidTill id validTill = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause validTill currTime)
    (predicate id)
  where
    setClause scValidTill currTime Storage.Case {..} =
      mconcat
        [ _validTill <-. B.val_ scValidTill,
          _updatedAt <-. B.val_ currTime
        ]
    predicate cid Storage.Case {..} = _id ==. B.val_ cid

updateStatus :: CaseId -> Storage.CaseStatus -> Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    setClause pStatus currTime Storage.Case {..} =
      mconcat
        [ _status <-. B.val_ pStatus,
          _updatedAt <-. B.val_ currTime
        ]
    predicate cid Storage.Case {..} = _id ==. B.val_ cid

updateStatusAndUdfs :: CaseId -> Storage.CaseStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow (T.DBResult ())
updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5 = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status udf1 udf2 udf3 udf4 udf5 currTime)
    (predicate id)
  where
    setClause pStatus pudf1 pudf2 pudf3 pudf4 pudf5 currTime Storage.Case {..} =
      mconcat
        [ _status <-. B.val_ pStatus,
          _updatedAt <-. B.val_ currTime,
          _udf1 <-. B.val_ pudf1,
          _udf2 <-. B.val_ pudf2,
          _udf3 <-. B.val_ pudf3,
          _udf4 <-. B.val_ pudf4,
          _udf5 <-. B.val_ pudf5
        ]
    predicate cid Storage.Case {..} = _id ==. B.val_ cid

findAllWithLimitOffsetWhere :: [Text] -> [Text] -> [Storage.CaseType] -> [Storage.CaseStatus] -> [Text] -> Maybe Int -> Maybe Int -> Flow (T.DBResult [Storage.Case])
findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset =
  DB.findAllWithLimitOffsetWhere
    dbTable
    predicate
    limit
    offset
    orderByDesc
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _fromLocationId `B.in_` (B.val_ <$> fromLocationIds) ||. complementVal fromLocationIds,
          _toLocationId `B.in_` (B.val_ <$> toLocationIds) ||. complementVal toLocationIds,
          _status `B.in_` (B.val_ <$> statuses) ||. complementVal statuses,
          _type `B.in_` (B.val_ <$> types) ||. complementVal types,
          _udf1 `B.in_` (B.val_ . Just <$> udf1s) ||. complementVal udf1s
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False
