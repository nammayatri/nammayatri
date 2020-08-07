module Beckn.Storage.Queries.Trail where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Config as DB
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Trail as Storage
import qualified Beckn.Utils.Servant.Trail.Server as Trail
import Data.Time.Units (Millisecond)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as BP
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T

getDbTable :: (L.MonadFlow mFlow, HasDbEnv mFlow) => mFlow (B.DatabaseEntity be DB.TrailDb (B.TableEntity Storage.TrailT))
getDbTable = do
  dbEnv <- getDbEnv
  let schemaName = dbSchema dbEnv
  pure $ DB._trail (DB.trailDb schemaName)

create :: (L.MonadFlow mFlow, HasDbEnv mFlow) => Storage.Trail -> mFlow (T.DBResult ())
create session = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression session)

setResponseInfo ::
  (L.MonadFlow mFlow, HasDbEnv mFlow) =>
  Text ->
  Millisecond ->
  Trail.ResponseInfo ->
  mFlow (T.DBResult ())
setResponseInfo reqId duration resp = do
  dbTable <- getDbTable
  DB.update dbTable setClause predicate
  where
    predicate :: Storage.TrailT (B.QExpr BP.Postgres s) -> B.QExpr BP.Postgres s Bool
    predicate Storage.Trail {..} = _id ==. B.val_ reqId
    setClause :: forall s. Storage.TrailT (B.QField s) -> B.QAssignment BP.Postgres s
    setClause Storage.Trail {..} =
      mconcat
        [ _succeeded <-. B.val_ (Just $ Trail._responseSucceeded resp),
          _responseBody <-. B.val_ (Just . decodeUtf8 $ Trail._responseBody resp),
          _responseStatus <-. B.val_ (Just $ Trail._responseStatus resp),
          _responseHeaders <-. B.val_ (Just $ Trail._responseHeadersString resp),
          _processDuration <-. B.val_ (Just duration)
        ]
