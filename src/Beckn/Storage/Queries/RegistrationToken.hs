{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.RegistrationToken where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.DB as DB
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable ::
     B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.RegistrationTokenT)
dbTable = DB._registrationToken DB.becknDb

findRegistrationToken ::
     Text -> L.Flow (T.DBResult (Maybe Storage.RegistrationToken))
findRegistrationToken id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.RegistrationToken {..} = (_id ==. B.val_ id)
