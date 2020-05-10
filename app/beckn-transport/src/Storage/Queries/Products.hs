module Storage.Queries.Products where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.App
import qualified Beckn.Types.Storage.Products as Storage
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductsT)
dbTable = DB._products DB.transporterDb

create :: Storage.Products -> L.Flow ()
create Storage.Products {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})
    >>= either DB.throwDBError pure

findAllByTypeOrgId :: Text -> Storage.ProductsStatus -> L.Flow [Storage.Products]
findAllByTypeOrgId orgId status =
  DB.findAll dbTable (predicate orgId status)
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Products {..} = B.desc_ _createdAt
    predicate orgId status Storage.Products {..} =
      ( _status ==. (B.val_ status)
          &&. _organizationId ==. (B.val_ orgId))


