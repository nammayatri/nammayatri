module Beckn.Storage.Queries.ExternalTrail where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Config as DB
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.ExternalTrail as Storage
import Beckn.Utils.Common (getSchemaName)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T

getDbTable :: DB.FlowWithDb r (B.DatabaseEntity be DB.TrailDb (B.TableEntity Storage.ExternalTrailT))
getDbTable =
  DB._externalTrail . DB.trailDb <$> getSchemaName

create :: Storage.ExternalTrail -> DB.FlowWithDb r (T.DBResult ())
create session = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression session)
