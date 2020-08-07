module Beckn.Storage.Queries.ExternalTrail where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Config as DB
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.ExternalTrail as Storage
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T

getDbTable :: (L.MonadFlow mFlow, HasDbEnv mFlow) => mFlow (B.DatabaseEntity be DB.TrailDb (B.TableEntity Storage.ExternalTrailT))
getDbTable = do
  dbEnv <- getDbEnv
  let schemaName = dbSchema dbEnv
  pure $ DB._externalTrail (DB.trailDb schemaName)

create :: (L.MonadFlow mFlow, HasDbEnv mFlow) => Storage.ExternalTrail -> mFlow (T.DBResult ())
create session = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression session)
