module Epass.Storage.Queries.CustomerDetail where

import Beckn.Types.Common
import Beckn.Utils.Extra
import Data.Aeson (Value (Null))
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Storage.CustomerDetail as Storage
import qualified Epass.Types.Storage.DB as DB
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Servant
import qualified Storage.Queries as DB

dbTable ::
  B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.CustomerDetailT)
dbTable = DB._CustomerDetail DB.becknDb

create :: Storage.CustomerDetail -> Flow ()
create Storage.CustomerDetail {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.CustomerDetail {..})
    >>= either DB.throwDBError pure

findById ::
  CustomerDetailId -> Flow (T.DBResult (Maybe Storage.CustomerDetail))
findById id =
  DB.findOne dbTable predicate
  where
    predicate Storage.CustomerDetail {..} = _id ==. B.val_ id

findByIdentifier ::
  Storage.IdentifierType -> Text -> Flow (Maybe Storage.CustomerDetail)
findByIdentifier idType mb =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.CustomerDetail {..} =
      _identifierType ==. B.val_ idType
        &&. _uniqueIdentifier ==. B.val_ mb

findAllByCustomerId ::
  CustomerId -> Flow [Storage.CustomerDetail]
findAllByCustomerId id =
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.CustomerDetail {..} = _CustomerId ==. B.val_ id

findExact :: CustomerId -> Storage.IdentifierType -> Text -> Flow (Maybe Storage.CustomerDetail)
findExact customerId idType identifier =
  DB.findOne dbTable (predicate customerId idType identifier)
    >>= either DB.throwDBError pure
  where
    predicate customerId idType identifier Storage.CustomerDetail {..} =
      _CustomerId ==. B.val_ customerId
        &&. _identifierType ==. B.val_ idType
        &&. _uniqueIdentifier ==. B.val_ identifier

createIfNotExistsCustomerD :: Storage.CustomerDetail -> Flow ()
createIfNotExistsCustomerD cust = do
  let cid = _uniqueIdentifier cust
  resp <- DB.findOne dbTable (\CustomerDetail {..} -> B.val_ cid ==. _uniqueIdentifier)
  case resp of
    Right (Just x) -> return ()
    Right Nothing -> create cust
    Left err -> L.throwException err500

createIfNotExists :: CustomerId -> Storage.IdentifierType -> Text -> Flow ()
createIfNotExists customerId idType identifier = do
  res <- findExact customerId idType identifier
  case res of
    Just _ -> return ()
    Nothing -> do
      details <- findAllByCustomerId customerId
      let isPrimary = null details
      create =<< getDetails isPrimary
  where
    getDetails isPrimary = do
      id <- generateGUID
      now <- getCurrentTimeUTC
      return $
        Storage.CustomerDetail
          { _id = id,
            _CustomerId = customerId,
            _uniqueIdentifier = identifier,
            _identifierType = idType,
            _value = Null,
            _verified = False,
            _primaryIdentifier = isPrimary,
            _info = "",
            _createdAt = now,
            _updatedAt = now
          }
