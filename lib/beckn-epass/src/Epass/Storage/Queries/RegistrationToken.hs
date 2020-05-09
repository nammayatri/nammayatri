{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.RegistrationToken where

import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import qualified Epass.Storage.Queries as DB
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.RegistrationToken as Storage
import Epass.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Servant

dbTable ::
  B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.RegistrationTokenT)
dbTable = DB._registrationToken DB.becknDb

create :: Storage.RegistrationToken -> L.Flow ()
create Storage.RegistrationToken {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})
    >>= either DB.throwDBError pure

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
  DB.update dbTable (setClause attemps now) (predicate id)
    >>= either DB.throwDBError pure
  findRegistrationToken id >>= maybe (L.throwException err500) pure
  where
    predicate i Storage.RegistrationToken {..} = (_id ==. B.val_ i)
    setClause a n Storage.RegistrationToken {..} =
      mconcat [_attempts <-. B.val_ a, _updatedAt <-. B.val_ n]
