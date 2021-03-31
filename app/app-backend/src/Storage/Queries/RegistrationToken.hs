{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB

getDbTable ::
  Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.RegistrationTokenT))
getDbTable =
  DB._registrationToken . DB.appDb <$> getSchemaName

create :: Storage.RegistrationToken -> Flow ()
create Storage.RegistrationToken {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})
    >>= either throwDBError pure

findById :: Text -> Flow (Maybe Storage.RegistrationToken)
findById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate >>= either throwDBError pure
  where
    predicate Storage.RegistrationToken {..} = _id ==. B.val_ id

findByToken :: Text -> Flow (Maybe Storage.RegistrationToken)
findByToken token = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate token)
    >>= either throwDBError pure
  where
    predicate rtoken Storage.RegistrationToken {..} = _token ==. B.val_ rtoken

updateAttempts :: Int -> Text -> Flow Storage.RegistrationToken
updateAttempts attemps id = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause attemps now) (predicate id)
    >>= either throwDBError pure
  findById id >>= fromMaybeM InvalidToken
  where
    predicate i Storage.RegistrationToken {..} = _id ==. B.val_ i
    setClause a n Storage.RegistrationToken {..} =
      mconcat [_attempts <-. B.val_ a, _updatedAt <-. B.val_ n]

deleteByPersonId :: Text -> Flow ()
deleteByPersonId id = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate id)
    >>= either throwDBError pure
  where
    predicate rtid Storage.RegistrationToken {..} = _EntityId ==. B.val_ rtid
