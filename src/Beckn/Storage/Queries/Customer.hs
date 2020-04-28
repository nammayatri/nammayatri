module Beckn.Storage.Queries.Customer where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Customer as C
import qualified Beckn.Types.Storage.DB as DB
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import Servant

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity C.CustomerT)
dbTable = DB._customer DB.becknDb

create :: C.Customer -> L.Flow ()
create C.Customer {..} =
  DB.createOne dbTable (C.insertExpression C.Customer {..}) >>=
  either DB.throwDBError pure

createIfNotExists :: C.Customer -> L.Flow ()
createIfNotExists cust@C.Customer {..} = do
  let cid = _id
  resp <- DB.findOne dbTable (\C.Customer {..} -> B.val_ cid ==. _id)
  case resp of
    Right (Just x) -> return ()
    Right Nothing -> create cust
    Left err -> L.throwException err500

existsByCustomerId :: CustomerId -> L.Flow Bool
existsByCustomerId customerId =
  DB.findOne dbTable predicate >>= \case
    Left err -> pure False
    Right Nothing -> pure False
    Right (Just j) -> pure True
  where
    predicate C.Customer {..} = _id ==. B.val_ customerId

findCustomerById :: CustomerId -> L.Flow (Maybe C.Customer)
findCustomerById customerId =
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
 where
   predicate C.Customer {..} = _id ==. B.val_ customerId

updateCustomerOrgId :: OrganizationId -> CustomerId -> L.Flow ()
updateCustomerOrgId orgId customerId =
  DB.update dbTable (setClause orgId) (predicate customerId)
    >>= either DB.throwDBError pure
  where
    setClause a C.Customer {..} = mconcat [ _OrganizationId <-. B.val_ (Just a) ]
    predicate i C.Customer {..} = _id ==. B.val_ i
