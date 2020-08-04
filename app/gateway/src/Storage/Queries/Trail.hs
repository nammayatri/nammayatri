module Storage.Queries.Trail where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.Trail as Storage
import qualified Beckn.Utils.Servant.Trail.Server as Trail
import Data.Time.Units (Millisecond)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as BP
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.TrailT)
dbTable = DB._trail DB.appDb

create :: Storage.Trail -> Flow (T.DBResult ())
create session =
  DB.createOne dbTable (Storage.insertExpression session)

setResponseInfo ::
  Text ->
  Millisecond ->
  Trail.ResponseInfo ->
  Flow (T.DBResult ())
setResponseInfo reqId duration resp = DB.update dbTable setClause predicate
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
