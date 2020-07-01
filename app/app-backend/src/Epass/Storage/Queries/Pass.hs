module Epass.Storage.Queries.Pass where

import Beckn.Utils.Extra (getCurrentTimeUTC)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import Epass.Types.App
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.Pass as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

data ListById
  = ByApplicationId PassApplicationId
  | ById PassId
  | ByCustomerId CustomerId
  | ByOrganizationId OrganizationId

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.PassT)
dbTable = DB._pass DB.becknDb

create :: Storage.Pass -> L.Flow ()
create Storage.Pass {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Pass {..})
    >>= either DB.throwDBError pure

findPassById :: Text -> L.Flow (Maybe Storage.Pass)
findPassById id =
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.Pass {..} = _ShortId ==. B.val_ id ||. _id ==. B.val_ (PassId id)

revokeByPassApplicationId :: PassApplicationId -> L.Flow ()
revokeByPassApplicationId passApplicationId =
  DB.update dbTable setClause (predicate passApplicationId)
    >>= either DB.throwDBError pure
  where
    predicate i Storage.Pass {..} = _PassApplicationId ==. B.val_ i
    setClause Storage.Pass {..} = mconcat [_status <-. B.val_ Storage.REVOKED]

updatePassStatus :: Storage.Status -> Text -> L.Flow ()
updatePassStatus action id =
  DB.update dbTable (setClause action) (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate i Storage.Pass {..} = _ShortId ==. B.val_ i
    setClause s Storage.Pass {..} = mconcat [_status <-. B.val_ s]

listAllPassesWithOffset :: Integer -> Integer -> ListById -> [Storage.Status] -> L.Flow [Storage.Pass]
listAllPassesWithOffset limit offset id stats =
  DB.findAllWithLimitOffsetWhere dbTable (predicate id stats) limit offset orderBy
    >>= either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.Pass {..} = _PassApplicationId ==. B.val_ i
    predicate (ByApplicationId i) s Storage.Pass {..} = _PassApplicationId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)
    predicate (ByCustomerId i) [] Storage.Pass {..} = _CustomerId ==. B.val_ (Just i)
    predicate (ByCustomerId i) s Storage.Pass {..} = _CustomerId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s)
    predicate (ByOrganizationId i) [] Storage.Pass {..} = _OrganizationId ==. B.val_ (Just i)
    predicate (ByOrganizationId i) s Storage.Pass {..} = _OrganizationId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s)
    predicate (ById i) [] Storage.Pass {..} = _id ==. B.val_ i
    predicate (ById i) s Storage.Pass {..} = _id ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)
    orderBy Storage.Pass {..} = B.desc_ _updatedAt

listAllPasses :: ListById -> [Storage.Status] -> L.Flow [Storage.Pass]
listAllPasses id status =
  DB.findAll dbTable (predicate id status)
    >>= either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.Pass {..} = _PassApplicationId ==. B.val_ i
    predicate (ByApplicationId i) s Storage.Pass {..} = _PassApplicationId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)
    predicate (ByCustomerId i) [] Storage.Pass {..} = _CustomerId ==. B.val_ (Just i)
    predicate (ByCustomerId i) s Storage.Pass {..} = _CustomerId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s)
    predicate (ByOrganizationId i) [] Storage.Pass {..} = _OrganizationId ==. B.val_ (Just i)
    predicate (ByOrganizationId i) s Storage.Pass {..} = _OrganizationId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s)
    predicate (ById i) [] Storage.Pass {..} = _id ==. B.val_ i
    predicate (ById i) s Storage.Pass {..} = _id ==. B.val_ i &&. B.in_ _status (B.val_ <$> s)

updateMultiple :: Text -> Storage.Pass -> L.Flow ()
updateMultiple id pass@Storage.Pass {..} = do
  currTime <- getCurrentTimeUTC
  DB.update dbTable (setClause currTime pass) (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate id Storage.Pass {..} = _ShortId ==. B.val_ id ||. _id ==. B.val_ (PassId id)
    setClause now pass Storage.Pass {..} =
      mconcat
        [ _updatedAt <-. B.val_ now,
          _status <-. B.val_ (Storage._status pass),
          _CustomerId <-. B.val_ (Storage._CustomerId pass),
          _fromLocationType <-. B.val_ (Storage._fromLocationType pass),
          _fromLat <-. B.val_ (Storage._fromLat pass),
          _fromLong <-. B.val_ (Storage._fromLong pass),
          _fromWard <-. B.val_ (Storage._fromWard pass),
          _fromDistrict <-. B.val_ (Storage._fromDistrict pass),
          _fromCity <-. B.val_ (Storage._fromCity pass),
          _fromState <-. B.val_ (Storage._fromState pass),
          _fromCountry <-. B.val_ (Storage._fromCountry pass),
          _fromPincode <-. B.val_ (Storage._fromPincode pass),
          _fromAddress <-. B.val_ (Storage._fromAddress pass),
          _fromBound <-. B.val_ (Storage._fromBound pass),
          _toLocationType <-. B.val_ (Storage._toLocationType pass),
          _toLat <-. B.val_ (Storage._toLat pass),
          _toLong <-. B.val_ (Storage._toLong pass),
          _toWard <-. B.val_ (Storage._toWard pass),
          _toDistrict <-. B.val_ (Storage._toDistrict pass),
          _toCity <-. B.val_ (Storage._toCity pass),
          _toState <-. B.val_ (Storage._toState pass),
          _toCountry <-. B.val_ (Storage._toCountry pass),
          _toPincode <-. B.val_ (Storage._toPincode pass),
          _toAddress <-. B.val_ (Storage._toAddress pass),
          _toBound <-. B.val_ (Storage._toBound pass)
        ]
