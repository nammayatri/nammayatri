{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import Beckn.Utils.Extra
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Servant
import qualified Types.Storage.DB as DB

dbTable ::
  B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.RegistrationTokenT)
dbTable = DB._registrationToken DB.appDb

create :: Storage.RegistrationToken -> Flow ()
create Storage.RegistrationToken {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})
    >>= either DB.throwDBError pure

findById :: Text -> Flow (Maybe Storage.RegistrationToken)
findById id =
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.RegistrationToken {..} = _id ==. B.val_ id

findByToken :: Text -> Flow (Maybe Storage.RegistrationToken)
findByToken token =
  DB.findOne dbTable (predicate token)
    >>= either DB.throwDBError pure
  where
    predicate rtoken Storage.RegistrationToken {..} = _token ==. B.val_ rtoken

updateAttempts :: Int -> Text -> Flow Storage.RegistrationToken
updateAttempts attemps id = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause attemps now) (predicate id)
    >>= either DB.throwDBError pure
  findById id >>= maybe (L.throwException err500) pure
  where
    predicate i Storage.RegistrationToken {..} = _id ==. B.val_ i
    setClause a n Storage.RegistrationToken {..} =
      mconcat [_attempts <-. B.val_ a, _updatedAt <-. B.val_ n]

deleteByPersonId :: Text -> Flow ()
deleteByPersonId id =
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate rtid Storage.RegistrationToken {..} = _EntityId ==. B.val_ rtid
