module Storage.Queries.NotificationStatus where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.ID (ID)
import Beckn.Utils.Common (getCurrTime, getSchemaName)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.NotificationStatus as NotificationStatus

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity NotificationStatus.NotificationStatusT))
getDbTable =
  DB._notificationStatus . DB.transporterDb <$> getSchemaName

create :: NotificationStatus.NotificationStatus -> Flow ()
create NotificationStatus.NotificationStatus {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression NotificationStatus.NotificationStatus {..})
    >>= either DB.throwDBError pure

updateStatus :: ID NotificationStatus.NotificationStatus -> NotificationStatus.AnswerStatus -> Flow ()
updateStatus notificationStatusId status = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause status now) (predicate notificationStatusId)
    >>= either DB.throwDBError pure
  where
    setClause s now NotificationStatus.NotificationStatus {..} =
      mconcat
        [ _status <-. B.val_ s,
          _updatedAt <-. B.val_ now
        ]
    predicate id NotificationStatus.NotificationStatus {..} = _id ==. B.val_ id

fetchStatusById :: ID NotificationStatus.NotificationStatus -> Flow (Maybe NotificationStatus.NotificationStatus)
fetchStatusById notificationStatusId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate NotificationStatus.NotificationStatus {..} = _id ==. B.val_ notificationStatusId
