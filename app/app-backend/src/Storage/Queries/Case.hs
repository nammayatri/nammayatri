{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Case where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Storage
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Storage.Queries.Person as Person
import Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.appDb

create :: Storage.Case -> Flow ()
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})
    >>= either DB.throwDBError pure

findAllByTypeAndStatuses ::
  PersonId ->
  Storage.CaseType ->
  [Storage.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  Flow [Storage.Case]
findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset =
  let limit = fromMaybe 100 mlimit
      offset = fromMaybe 0 moffset
   in DB.findAllWithLimitOffsetWhere dbTable (predicate personId caseType caseStatuses) limit offset orderByDesc
        >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate personId caseType caseStatuses Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _type ==. B.val_ caseType,
          B.in_ _status (B.val_ <$> caseStatuses) ||. complementVal caseStatuses,
          _requestor ==. B.val_ (Just $ _getPersonId personId)
        ]

findById :: CaseId -> Flow Storage.Case
findById caseId =
  DB.findOneWithErr dbTable (predicate caseId)
  where
    predicate caseId Storage.Case {..} = _id ==. B.val_ caseId

findByIdAndType :: CaseId -> Storage.CaseType -> Flow Storage.Case
findByIdAndType caseId caseType =
  DB.findOneWithErr dbTable (predicate caseId caseType)
  where
    predicate caseId caseType Storage.Case {..} =
      (_id ==. B.val_ caseId)
        &&. (_type ==. B.val_ caseType)

findIdByPerson :: Person.Person -> CaseId -> Flow Storage.Case
findIdByPerson person caseId = do
  let personId = _getPersonId $ person ^. #_id
  DB.findOneWithErr dbTable (predicate personId caseId)
  where
    predicate personId caseId Storage.Case {..} =
      _id ==. B.val_ caseId &&. _requestor ==. B.val_ (Just personId)

findAllByIds :: [CaseId] -> Flow [Storage.Case]
findAllByIds caseIds =
  if null caseIds
    then return []
    else
      DB.findAll dbTable (predicate caseIds)
        >>= either DB.throwDBError pure
  where
    predicate caseIds Storage.Case {..} = _id `B.in_` (B.val_ <$> caseIds)

findAllByPerson :: Text -> Flow [Storage.Case]
findAllByPerson perId =
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Case {..} = _requestor ==. B.val_ (Just perId)

findAllExpiredByStatus :: [Storage.CaseStatus] -> Maybe LocalTime -> Maybe LocalTime -> Flow [Storage.Case]
findAllExpiredByStatus statuses maybeFrom maybeTo = do
  (now :: LocalTime) <- getCurrentTimeUTC
  DB.findAll dbTable (predicate now maybeFrom maybeTo)
    >>= either DB.throwDBError pure
  where
    predicate now maybeFrom maybeTo Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        ( [ _status `B.in_` (B.val_ <$> statuses),
            _validTill B.<=. B.val_ now
          ]
            <> maybe [] (\from -> [_createdAt B.>=. B.val_ from]) maybeFrom
            <> maybe [] (\to -> [_createdAt B.<=. B.val_ to]) maybeTo
        )

updateValidTill :: CaseId -> LocalTime -> Flow ()
updateValidTill id validTill = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause validTill currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause validTill currTime Storage.Case {..} =
      mconcat
        [ _validTill <-. B.val_ validTill,
          _updatedAt <-. B.val_ currTime
        ]
    predicate id Storage.Case {..} = _id ==. B.val_ id

updateStatus :: CaseId -> Storage.CaseStatus -> Flow ()
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
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

updateStatusAndUdfs :: CaseId -> Storage.CaseStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow ()
updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5 = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status udf1 udf2 udf3 udf4 udf5 currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status udf1 udf2 udf3 udf4 udf5 currTime Storage.Case {..} =
      mconcat
        [ _status <-. B.val_ status,
          _updatedAt <-. B.val_ currTime,
          _udf1 <-. B.val_ udf1,
          _udf2 <-. B.val_ udf2,
          _udf3 <-. B.val_ udf3,
          _udf4 <-. B.val_ udf4,
          _udf5 <-. B.val_ udf5
        ]
    predicate id Storage.Case {..} = _id ==. B.val_ id

findAllWithLimitOffsetWhere :: [Text] -> [Text] -> [Storage.CaseType] -> [Storage.CaseStatus] -> [Text] -> Maybe Int -> Maybe Int -> Flow [Storage.Case]
findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset =
  DB.findAllWithLimitOffsetWhere
    dbTable
    (predicate fromLocationIds toLocationIds types statuses udf1s)
    limit
    offset
    orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate fromLocationIds toLocationIds types statuses udf1s Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _fromLocationId `B.in_` (B.val_ <$> fromLocationIds) ||. complementVal fromLocationIds,
          _toLocationId `B.in_` (B.val_ <$> toLocationIds) ||. complementVal toLocationIds,
          _status `B.in_` (B.val_ <$> statuses) ||. complementVal statuses,
          _type `B.in_` (B.val_ <$> types) ||. complementVal types,
          _udf1 `B.in_` (B.val_ . Just <$> udf1s) ||. complementVal udf1s
        ]

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False
