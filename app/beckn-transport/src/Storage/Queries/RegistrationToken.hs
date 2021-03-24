{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB

getDbTable ::
  Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RegistrationTokenT))
getDbTable =
  DB._registrationToken . DB.transporterDb <$> getSchemaName

create :: Storage.RegistrationToken -> Flow ()
create Storage.RegistrationToken {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})
    >>= either throwDBError pure

findRegistrationToken :: Text -> Flow (Maybe Storage.RegistrationToken)
findRegistrationToken id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate >>= either throwDBError pure
  where
    predicate Storage.RegistrationToken {..} = _id ==. B.val_ id

updateVerified :: Text -> Bool -> Flow ()
updateVerified id verified = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause verified now) (predicate id)
    >>= either throwDBError pure
  where
    setClause scVerified currTime Storage.RegistrationToken {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _verified <-. B.val_ scVerified
        ]
    predicate rtid Storage.RegistrationToken {..} = _id ==. B.val_ rtid

verifyToken :: RegToken -> Flow Storage.RegistrationToken
verifyToken regToken = do
  logInfo "verifying token" $ show regToken
  findRegistrationTokenByToken regToken

findRegistrationTokenByToken :: RegToken -> Flow Storage.RegistrationToken
findRegistrationTokenByToken regToken = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate regToken)
    >>= either throwDBError pure
    >>= fromMaybeM InvalidToken
  where
    predicate token Storage.RegistrationToken {..} = _token ==. B.val_ token

updateAttempts :: Int -> Text -> Flow Storage.RegistrationToken
updateAttempts attemps id = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause attemps now) (predicate id)
    >>= either throwDBError pure
  findRegistrationToken id >>= fromMaybeM InvalidToken
  where
    predicate i Storage.RegistrationToken {..} = _id ==. B.val_ i
    setClause a n Storage.RegistrationToken {..} =
      mconcat [_attempts <-. B.val_ a, _updatedAt <-. B.val_ n]

deleteByEntitiyId :: Text -> Flow ()
deleteByEntitiyId id = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate id)
    >>= either throwDBError pure
  where
    predicate rtid Storage.RegistrationToken {..} = _EntityId ==. B.val_ rtid
