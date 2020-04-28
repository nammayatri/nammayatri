module Beckn.Storage.Queries.Pass where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.DB as DB
import qualified Beckn.Types.Storage.Pass as Storage
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.PassT)
dbTable = DB._pass DB.becknDb

create :: Storage.Pass -> L.Flow ()
create Storage.Pass {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Pass {..}) >>=
  either DB.throwDBError pure

findPassById :: PassId -> L.Flow (Maybe Storage.Pass)
findPassById id = do
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.Pass {..} = (_id ==. B.val_ id)

updatePassStatus :: Storage.Status -> PassId -> L.Flow ()
updatePassStatus action id = do
  DB.update dbTable (setClause action) (predicate id) >>=
    either DB.throwDBError pure
  where
    predicate i Storage.Pass {..} = (_id ==. B.val_ i)
    setClause s Storage.Pass {..} = mconcat [ _status <-. B.val_ s ]

listAllPassesWithOffset :: Integer -> Integer -> PassId -> [Storage.Status]-> L.Flow [Storage.Pass]
listAllPassesWithOffset limit offset id stats = do
  DB.findAllWithLimitOffsetWhere dbTable (predicate id stats) limit offset orderBy >>=
    either DB.throwDBError pure
  where
    predicate i [] Storage.Pass {..} = (_id ==. B.val_ i)
    predicate i s Storage.Pass {..} = (_id ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    orderBy Storage.Pass {..} = B.desc_ _updatedAt

listAllPasses :: PassId -> [Storage.Status] -> L.Flow [Storage.Pass]
listAllPasses id status = do
  DB.findAll dbTable (predicate id status) >>=
    either DB.throwDBError pure
  where
    predicate i [] Storage.Pass {..} = (_id ==. B.val_ i)
    predicate i s Storage.Pass {..} = (_id ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
