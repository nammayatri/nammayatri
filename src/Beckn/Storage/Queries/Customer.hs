module Beckn.Storage.Queries.Customer where

import           Database.Beam                ((&&.), (<-.), (==.))
import           EulerHS.Prelude              hiding (id)

import qualified Beckn.Storage.Queries        as DB
import           Beckn.Types.App
import qualified Beckn.Types.Domain.Customer  as CD
import qualified Beckn.Types.Storage.Customer as C
import qualified Beckn.Types.Storage.DB       as DB
import qualified Database.Beam                as B
import qualified EulerHS.Language             as L

dbTable :: B.DatabaseEntity be DB.BecknDB (B.TableEntity C.CustomerT)
dbTable = DB._customer DB.becknDB

create :: CD.Customer -> L.Flow ()
create CD.Customer{..} =
  DB.createOne dbTable (C.insertExpression C.Customer{..}) >>=
  either DB.throwDBError pure

existsByCustomerId :: CustomerId -> L.Flow Bool
existsByCustomerId customerId =
  DB.findOne dbTable predicate >>= \case
    Left err -> pure False
    Right Nothing -> pure False
    Right (Just j) -> pure True
  where
    predicate C.Customer {..} = _id ==. B.val_ customerId
