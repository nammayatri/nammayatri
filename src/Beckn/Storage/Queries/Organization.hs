module Beckn.Storage.Queries.Organization where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.DB as DB
import qualified Beckn.Types.Storage.Organization as Storage
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.OrganizationT)
dbTable = DB._organization DB.becknDb

create :: Storage.Organization -> L.Flow ()
create Storage.Organization {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..}) >>=
  either DB.throwDBError pure

findOrganizationById ::
     OrganizationId -> L.Flow (Maybe Storage.Organization)
findOrganizationById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Organization {..} = (_id ==. B.val_ id)
