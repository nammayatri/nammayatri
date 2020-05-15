{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import           Database.Beam                         ((&&.), (<-.), (==.))
import           EulerHS.Prelude                       hiding (id)

import qualified Storage.Queries                 as DB
import qualified Types.Storage.DB                as DB
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import           Beckn.Utils.Extra
import qualified Database.Beam                         as B
import qualified EulerHS.Language                      as L
import qualified EulerHS.Types                         as T
import           Servant
import Beckn.Utils.Common

dbTable ::
     B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RegistrationTokenT)
dbTable = DB._registrationToken DB.transporterDb

create :: Storage.RegistrationToken -> L.Flow ()
create Storage.RegistrationToken {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..}) >>=
  either DB.throwDBError pure

findRegistrationToken :: Text -> L.Flow (Maybe Storage.RegistrationToken)
findRegistrationToken id = do
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.RegistrationToken {..} = (_id ==. B.val_ id)

updateVerified :: Text -> Bool -> L.Flow ()
updateVerified id verified = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause verified now) (predicate id) >>=
    either DB.throwDBError pure
  where
    setClause verified currTime Storage.RegistrationToken {..} =
      mconcat
        ( [ _updatedAt <-. B.val_ currTime,
            _verified <-. B.val_ verified
          ]
        )
    predicate id Storage.RegistrationToken {..} = _id ==. B.val_ id

verifyAuth :: Maybe Text -> L.Flow Storage.RegistrationToken
verifyAuth auth = do
  L.logInfo "verifying auth" $ show auth
  let token = base64Decode auth
  -- did atob of auth by removing basic in front and after atob, `:` in the end
  L.logInfo "verifying token" $ show token
  findRegistrationTokenByToken token

findRegistrationTokenByToken :: Maybe Text -> L.Flow Storage.RegistrationToken
findRegistrationTokenByToken idM = do
  id <- fromMaybeM400 "INVALID_TOKEN" idM
  DB.findOne dbTable (predicate id) >>= either DB.throwDBError pure >>= fromMaybeM400 "INVALID_TOKEN"
  where
    predicate id Storage.RegistrationToken {..} = (_token ==. B.val_ id)

updateAttempts :: Int -> Text -> L.Flow Storage.RegistrationToken
updateAttempts attemps id = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause attemps now) (predicate id) >>=
    either DB.throwDBError pure
  findRegistrationToken id >>= maybe (L.throwException err500) pure
  where
    predicate i Storage.RegistrationToken {..} = (_id ==. B.val_ i)
    setClause a n Storage.RegistrationToken {..} =
      mconcat [_attempts <-. B.val_ a, _updatedAt <-. B.val_ n]
