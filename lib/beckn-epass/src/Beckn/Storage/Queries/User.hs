{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.User where

import           Database.Beam            ((&&.), (<-.), (==.))
import           EulerHS.Prelude          hiding (id)

import qualified Beckn.Storage.Queries    as DB
import           Beckn.Types.App
import qualified Beckn.Types.Storage.DB   as DB
import qualified Beckn.Types.Storage.User as Storage
import           Beckn.Utils.Common
import           Data.Time.LocalTime
import qualified Database.Beam            as B
import qualified EulerHS.Language         as L
import qualified EulerHS.Types            as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.UserT)
dbTable = DB._user DB.becknDb

create :: Storage.User -> L.Flow ()
create Storage.User {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.User {..}) >>=
  either DB.throwDBError pure

findById :: UserId -> L.Flow Storage.User
findById id = do
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.User {..} = (_id ==. B.val_ id)

findByMobileNumber :: Text -> L.Flow (Maybe Storage.User)
findByMobileNumber mobileNumber = do
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.User {..} = (_mobileNumber ==. B.val_ mobileNumber)

findAllWithLimitOffset :: Maybe Int -> Maybe Int -> L.Flow [Storage.User]
findAllWithLimitOffset mlimit moffset =
  DB.findAllWithLimitOffset dbTable limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 10 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)

    orderByDesc Storage.User {..} = B.desc_ _createdAt

findAllWithLimitOffsetByRole :: Maybe Int -> Maybe Int -> [Storage.Role] -> L.Flow [Storage.User]
findAllWithLimitOffsetByRole mlimit moffset roles =
  DB.findAllWithLimitOffsetWhere dbTable (predicate roles) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 10 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)

    predicate r Storage.User {..} =
      _role `B.in_` (B.val_ <$> r)

    orderByDesc Storage.User {..} = B.desc_ _createdAt

findAllWithLimitOffsetBy :: Maybe Int -> Maybe Int -> Maybe Storage.Role -> [OrganizationId] -> L.Flow [Storage.User]
findAllWithLimitOffsetBy mlimit moffset r f =
  DB.findAllWithLimitOffsetWhere dbTable (predicate f r) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 10 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)

    predicate i Nothing Storage.User {..} =
      _OrganizationId `B.in_` (B.val_ <$> i)
    predicate i (Just r) Storage.User {..} = _OrganizationId `B.in_` (B.val_ <$> i) &&. _role ==. B.val_ r

    orderByDesc Storage.User {..} = B.desc_ _createdAt

update ::
  UserId
  -> Storage.Status
  -> Maybe Text
  -> Maybe Text
  -> Maybe Storage.Role -> L.Flow ()
update id status nameM emailM roleM = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update dbTable
    (setClause status nameM emailM roleM currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status nameM emailM roleM currTime Storage.User {..} =
      mconcat
        ([ _status <-. B.val_ status
        , _updatedAt <-. B.val_ currTime
        ] <> maybe [] (\name -> [ _name <-. B.val_ name ]) nameM
          <> maybe [] (\email -> [ _email <-. B.val_ email ]) emailM
          <> maybe [] (\role -> [ _role <-. B.val_ role ]) roleM
        )

    predicate id Storage.User {..} = _id ==. B.val_ id

deleteById :: UserId -> L.Flow ()
deleteById id =
  DB.delete dbTable (predicate id)
  >>= either DB.throwDBError pure
  where
    predicate id Storage.User {..} = _id ==. B.val_ id
