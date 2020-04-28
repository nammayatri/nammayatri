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

data ListById
  = ByApplicationId PassApplicationId
  | ById PassId
  | ByCustomerId CustomerId
  | ByOrganizationId OrganizationId

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.PassT)
dbTable = DB._pass DB.becknDb

create :: Storage.Pass -> L.Flow ()
create Storage.Pass {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Pass {..}) >>=
  either DB.throwDBError pure

findPassById :: Text -> L.Flow (Maybe Storage.Pass)
findPassById id = do
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.Pass {..} = (_ShortId ==. B.val_ id)

updatePassStatus :: Storage.Status -> Text -> L.Flow ()
updatePassStatus action id = do
  DB.update dbTable (setClause action) (predicate id) >>=
    either DB.throwDBError pure
  where
    predicate i Storage.Pass {..} = (_ShortId ==. B.val_ i)
    setClause s Storage.Pass {..} = mconcat [ _status <-. B.val_ s ]

listAllPassesWithOffset :: Integer -> Integer -> ListById -> [Storage.Status]-> L.Flow [Storage.Pass]
listAllPassesWithOffset limit offset id stats = do
  DB.findAllWithLimitOffsetWhere dbTable (predicate id stats) limit offset orderBy >>=
    either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.Pass {..} = (_PassApplicationId ==. B.val_ i)
    predicate (ByApplicationId i) s Storage.Pass {..} = (_PassApplicationId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    predicate (ByCustomerId i) [] Storage.Pass {..} = (_CustomerId ==. B.val_ i)
    predicate (ByCustomerId i) s Storage.Pass {..} = (_CustomerId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    predicate (ByOrganizationId i) [] Storage.Pass {..} = (_OrganizationId ==. B.val_ (Just i))
    predicate (ByOrganizationId i) s Storage.Pass {..} = (_OrganizationId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s))
    predicate (ById i) [] Storage.Pass {..} = (_id ==. B.val_ i)
    predicate (ById i) s Storage.Pass {..} = (_id ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    orderBy Storage.Pass {..} = B.desc_ _updatedAt

listAllPasses :: ListById -> [Storage.Status] -> L.Flow [Storage.Pass]
listAllPasses id status = do
  DB.findAll dbTable (predicate id status) >>=
    either DB.throwDBError pure
  where
    predicate (ByApplicationId i) [] Storage.Pass {..} = (_PassApplicationId ==. B.val_ i)
    predicate (ByApplicationId i) s Storage.Pass {..} = (_PassApplicationId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    predicate (ByCustomerId i) [] Storage.Pass {..} = (_CustomerId ==. B.val_ i)
    predicate (ByCustomerId i) s Storage.Pass {..} = (_CustomerId ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
    predicate (ByOrganizationId i) [] Storage.Pass {..} = (_OrganizationId ==. B.val_ (Just i))
    predicate (ByOrganizationId i) s Storage.Pass {..} = (_OrganizationId ==. B.val_ (Just i) &&. B.in_ _status (B.val_ <$> s))
    predicate (ById i) [] Storage.Pass {..} = (_id ==. B.val_ i)
    predicate (ById i) s Storage.Pass {..} = (_id ==. B.val_ i &&. B.in_ _status (B.val_ <$> s))
