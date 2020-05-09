{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import           Database.Beam                         ((&&.), (<-.), (==.))
import           EulerHS.Prelude                       hiding (id)

import qualified Storage.Queries                 as DB
import qualified Types.Storage.DB                as DB
import qualified Epass.Types.Storage.RegistrationToken as Storage
import           Epass.Utils.Extra
import qualified Database.Beam                         as B
import qualified EulerHS.Language                      as L
import qualified EulerHS.Types                         as T
import           Servant

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

findRegistrationTokenByToken :: Text -> L.Flow (Maybe Storage.RegistrationToken)
findRegistrationTokenByToken id = do
  DB.findOne dbTable predicate >>= either DB.throwDBError pure
  where
    predicate Storage.RegistrationToken {..} = (_token ==. B.val_ id)

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
