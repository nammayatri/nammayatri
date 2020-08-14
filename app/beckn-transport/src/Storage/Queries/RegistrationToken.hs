{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RegistrationToken where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Servant
import qualified Types.Storage.DB as DB

dbTable ::
  B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RegistrationTokenT)
dbTable = DB._registrationToken DB.transporterDb

create :: Storage.RegistrationToken -> Flow ()
create Storage.RegistrationToken {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})
    >>= either DB.throwDBError pure

findRegistrationToken :: Text -> Flow (Maybe Storage.RegistrationToken)
findRegistrationToken id =
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.RegistrationToken {..} = _id ==. B.val_ id

updateVerified :: Text -> Bool -> Flow ()
updateVerified id verified = do
  now <- getCurrTime
  DB.update dbTable (setClause verified now) (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause scVerified currTime Storage.RegistrationToken {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _verified <-. B.val_ scVerified
        ]
    predicate rtid Storage.RegistrationToken {..} = _id ==. B.val_ rtid

verifyToken :: RegToken -> Flow Storage.RegistrationToken
verifyToken regToken = do
  L.logInfo @Text "verifying token" $ show regToken
  findRegistrationTokenByToken regToken

findRegistrationTokenByToken :: RegToken -> Flow Storage.RegistrationToken
findRegistrationTokenByToken regToken =
  DB.findOne dbTable (predicate regToken)
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_TOKEN"
  where
    predicate token Storage.RegistrationToken {..} = _token ==. B.val_ token

updateAttempts :: Int -> Text -> Flow Storage.RegistrationToken
updateAttempts attemps id = do
  now <- getCurrTime
  DB.update dbTable (setClause attemps now) (predicate id)
    >>= either DB.throwDBError pure
  findRegistrationToken id >>= maybe (L.throwException err500) pure
  where
    predicate i Storage.RegistrationToken {..} = _id ==. B.val_ i
    setClause a n Storage.RegistrationToken {..} =
      mconcat [_attempts <-. B.val_ a, _updatedAt <-. B.val_ n]

deleteByEntitiyId :: Text -> Flow ()
deleteByEntitiyId id =
  DB.delete dbTable (predicate id)
    >>= either DB.throwDBError pure
  where
    predicate rtid Storage.RegistrationToken {..} = _EntityId ==. B.val_ rtid
