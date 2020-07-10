module Epass.Storage.Queries.Document where

import Beckn.Types.Common
import Data.Time
import Data.Time.LocalTime
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.Document as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.DocumentT)
dbTable = DB._document DB.becknDb

create :: Storage.Document -> Flow ()
create Storage.Document {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Document {..})
    >>= either DB.throwDBError pure

findById :: DocumentId -> Flow (Maybe Storage.Document)
findById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Document {..} = _id ==. B.val_ id
