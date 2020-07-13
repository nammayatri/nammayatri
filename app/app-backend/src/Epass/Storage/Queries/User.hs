{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.User where

import App.Types
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Time.LocalTime
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import Epass.Types.App
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.User as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.UserT)
dbTable = DB._user DB.becknDb

create :: Storage.User -> Flow ()
create Storage.User {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.User {..})
    >>= either DB.throwDBError pure

findById :: UserId -> Flow Storage.User
findById id =
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.User {..} = _id ==. B.val_ id

findByMobileNumber :: Text -> Flow (Maybe Storage.User)
findByMobileNumber mobileNumber =
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.User {..} = _mobileNumber ==. B.val_ mobileNumber

findAllWithLimitOffset :: Maybe Int -> Maybe Int -> Flow [Storage.User]
findAllWithLimitOffset mlimit moffset =
  DB.findAllWithLimitOffset dbTable limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.User {..} = B.desc_ _createdAt

findAllWithLimitOffsetByRole :: Maybe Int -> Maybe Int -> [Storage.Role] -> Flow [Storage.User]
findAllWithLimitOffsetByRole mlimit moffset roles =
  DB.findAllWithLimitOffsetWhere dbTable (predicate roles) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate [] Storage.User {..} = B.val_ True
    predicate r Storage.User {..} =
      _role `B.in_` (B.val_ <$> r)
    orderByDesc Storage.User {..} = B.desc_ _createdAt

findAllWithLimitOffsetBy :: Maybe Int -> Maybe Int -> [Storage.Role] -> [OrganizationId] -> Flow [Storage.User]
findAllWithLimitOffsetBy mlimit moffset r f =
  DB.findAllWithLimitOffsetWhere dbTable (predicate f r) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    predicate i [] Storage.User {..} =
      _OrganizationId `B.in_` (B.val_ <$> i)
    predicate i r Storage.User {..} =
      _OrganizationId `B.in_` (B.val_ <$> i) &&. _role `B.in_` (B.val_ <$> r)
    orderByDesc Storage.User {..} = B.desc_ _createdAt

update ::
  UserId ->
  Storage.Status ->
  Maybe Text ->
  Maybe Storage.Role ->
  Flow ()
update id status nameM roleM = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status nameM roleM currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status nameM roleM currTime Storage.User {..} =
      mconcat
        ( [ _status <-. B.val_ status,
            _updatedAt <-. B.val_ currTime
          ]
            <> maybe [] (\name -> [_name <-. B.val_ name]) nameM
            <> maybe [] (\role -> [_role <-. B.val_ role]) roleM
        )
    predicate id Storage.User {..} = _id ==. B.val_ id

deleteById :: UserId -> Flow ()
deleteById id =
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.User {..} = _id ==. B.val_ id
