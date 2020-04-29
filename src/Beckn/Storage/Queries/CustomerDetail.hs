module Beckn.Storage.Queries.CustomerDetail where

import           Database.Beam                      ((&&.), (<-.), (==.))
import           EulerHS.Prelude                    hiding (id)

import qualified Beckn.Storage.Queries              as DB
import           Beckn.Types.App
import qualified Beckn.Types.Storage.CustomerDetail as Storage
import qualified Beckn.Types.Storage.DB             as DB
import qualified Database.Beam                      as B
import qualified EulerHS.Language                   as L
import qualified EulerHS.Types                      as T

dbTable ::
     B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.CustomerDetailT)
dbTable = DB._CustomerDetail DB.becknDb

create :: Storage.CustomerDetail -> L.Flow ()
create Storage.CustomerDetail {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.CustomerDetail {..}) >>=
  either DB.throwDBError pure

findCustomerDetailById ::
     CustomerDetailId -> L.Flow (T.DBResult (Maybe Storage.CustomerDetail))
findCustomerDetailById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.CustomerDetail {..} = (_id ==. B.val_ id)

findCustomerDetailByMB ::
  Text -> L.Flow (Maybe Storage.CustomerDetail)
findCustomerDetailByMB mb =
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.CustomerDetail {..} =
      _identifierType ==. B.val_ Storage.MOBILENUMBER &&.
      _uniqueIdentifier ==. B.val_ mb
