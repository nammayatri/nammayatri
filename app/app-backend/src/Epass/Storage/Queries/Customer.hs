module Epass.Storage.Queries.Customer where

import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import Epass.Types.App
import qualified Epass.Types.Storage.Customer as C
import qualified Epass.Types.Storage.DB as DB
import Epass.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Servant
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity C.CustomerT)
dbTable = DB._customer DB.becknDb

create :: C.Customer -> L.Flow ()
create C.Customer {..} =
  DB.createOne dbTable (C.insertExpression C.Customer {..})
    >>= either DB.throwDBError pure

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
updateCustomerOrgId orgId customerId = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause orgId now) (predicate customerId)
    >>= either DB.throwDBError pure
  where
    setClause a n C.Customer {..} =
      mconcat [_OrganizationId <-. B.val_ (Just a), _updatedAt <-. B.val_ n]
    predicate i C.Customer {..} = _id ==. B.val_ i

updateStatus :: Bool -> CustomerId -> L.Flow ()
updateStatus s customerId = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause s now) (predicate customerId)
    >>= either DB.throwDBError pure
  where
    setClause a n C.Customer {..} =
      mconcat [_verified <-. B.val_ a, _updatedAt <-. B.val_ n]
    predicate i C.Customer {..} = _id ==. B.val_ i

updateDetails :: CustomerId -> Maybe Text -> Maybe OrganizationId -> L.Flow ()
updateDetails customerId nameM orgIdM = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause nameM orgIdM now) (predicate customerId)
    >>= either DB.throwDBError pure
  where
    setClause nameM orgIdM now C.Customer {..} =
      mconcat
        ( [_updatedAt <-. B.val_ now]
            <> maybe [] (\name -> [_name <-. B.val_ (Just name)]) nameM
            <> maybe [] (\orgId -> [_OrganizationId <-. B.val_ (Just orgId)]) orgIdM
        )
    predicate id C.Customer {..} = _id ==. B.val_ id

--updateDocument :: CustomerId -> [DocumentId] -> L.Flow ()
--updateDocument cust docs =
--DB.update dbTable (setClause docs) (predicate cust)
-- >>= either DB.throwDBError pure
--where
--setClause docs C.Customer {..} =
--mconcat [ _documentIds <-. B.val_ docs ]
--predicate i C.Customer {..} = _id ==. B.val_ i
