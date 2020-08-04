module Storage.Queries.ExternalTrail where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.ExternalTrail as Storage
import qualified Database.Beam as B
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ExternalTrailT)
dbTable = DB._externalTrail DB.appDb

create :: Storage.ExternalTrail -> Flow (T.DBResult ())
create session =
  DB.createOne dbTable (Storage.insertExpression session)
