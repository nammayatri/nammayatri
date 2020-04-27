{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.User where

import           Database.Beam            ((&&.), (<-.), (==.))
import           EulerHS.Prelude          hiding (id)

import qualified Beckn.Storage.Queries    as DB
import qualified Beckn.Types.Domain.User  as Domain
import qualified Beckn.Types.Storage.DB   as DB
import qualified Beckn.Types.Storage.User as Storage
import qualified Database.Beam            as B
import qualified EulerHS.Language         as L
import qualified EulerHS.Types            as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.UserT)
dbTable = DB._user DB.becknDb

findUser :: Text -> L.Flow (T.DBResult (Maybe Domain.User))
findUser id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.User {..} = (_id ==. B.val_ id)
