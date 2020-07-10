module Epass.Storage.Queries.PassApplication where

import Beckn.Types.Common
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Time.LocalTime
import Database.Beam
  ( (&&.),
    (<-.),
    (==.),
    (||.),
  )
import qualified Database.Beam as B
import Epass.Types.App
import qualified Epass.Types.Common as Storage (PassType (..))
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.PassApplication as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable ::
  B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.PassApplicationT)
dbTable = DB._passApplication DB.becknDb

create :: Storage.PassApplication -> Flow ()
create Storage.PassApplication {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.PassApplication {..})
    >>= either DB.throwDBError pure

findById ::
  PassApplicationId -> Flow (Maybe Storage.PassApplication)
findById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.PassApplication {..} = _id ==. B.val_ id

findAllWithLimitOffsetWhere ::
  [Int] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Int] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Storage.Status] ->
  [OrganizationId] ->
  [Storage.PassType] ->
  Maybe CustomerId ->
  Maybe Int ->
  Maybe Int ->
  Flow [Storage.PassApplication]
findAllWithLimitOffsetWhere
  fPins
  fCities
  fDists
  fWards
  fStates
  toPins
  toCities
  toDists
  toWards
  toStates
  statuses
  orgIds
  passType
  mCreatedById
  mlimit
  moffset =
    DB.findAllWithLimitOffsetWhere
      dbTable
      (predicate fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType mCreatedById)
      limit
      offset
      orderByDesc
      >>= either DB.throwDBError pure
    where
      limit = toInteger $ fromMaybe 100 mlimit
      offset = toInteger $ fromMaybe 0 moffset
      orderByDesc Storage.PassApplication {..} = B.desc_ _createdAt
      predicate
        fPins
        fCities
        fDists
        fWards
        fStates
        toPins
        toCities
        toDists
        toWards
        toStates
        statuses
        orgIds
        ptypes
        mCreatedById
        Storage.PassApplication {..} =
          foldl
            (&&.)
            (B.val_ True)
            [ _passType `B.in_` (B.val_ <$> ptypes) ||. complementVal ptypes,
              _status `B.in_` (B.val_ <$> statuses) ||. complementVal statuses,
              _OrganizationId `B.in_` (B.val_ . Just <$> orgIds) ||. complementVal orgIds,
              _toPincode `B.in_` (B.val_ . Just <$> toPins) ||. complementVal toPins,
              _toCity `B.in_` (B.val_ . Just <$> toCities) ||. complementVal toCities,
              _toState `B.in_` (B.val_ . Just <$> toStates) ||. complementVal toStates,
              _toDistrict `B.in_` (B.val_ . Just <$> toDists) ||. complementVal toDists,
              _toWard `B.in_` (B.val_ . Just <$> toWards) ||. complementVal toWards,
              _fromPincode `B.in_` (B.val_ . Just <$> fPins) ||. complementVal fPins,
              _fromCity `B.in_` (B.val_ . Just <$> fCities) ||. complementVal fCities,
              _fromState `B.in_` (B.val_ . Just <$> fStates) ||. complementVal fStates,
              _fromDistrict `B.in_` (B.val_ . Just <$> fDists) ||. complementVal fDists,
              _fromWard `B.in_` (B.val_ . Just <$> fWards) ||. complementVal fWards,
              maybe (B.val_ True) (\custId -> _CreatedBy ==. B.val_ custId) mCreatedById
            ]

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update :: PassApplicationId -> Storage.Status -> Maybe Int -> Maybe Text -> Flow ()
update id status approvedCountM remarksM = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status approvedCountM remarksM currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status approvedCountM remarksM currTime Storage.PassApplication {..} =
      mconcat
        ( [ _status <-. B.val_ status,
            _updatedAt <-. B.val_ currTime
          ]
            <> maybe [] (\remarks -> [_remarks <-. B.val_ remarks]) remarksM
            <> maybe [] (\approvedCount -> [_approvedCount <-. B.val_ approvedCount]) approvedCountM
        )
    predicate id Storage.PassApplication {..} = _id ==. B.val_ id
