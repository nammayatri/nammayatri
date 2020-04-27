{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.LocationBlacklist where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.DB as DB
import qualified Beckn.Types.Storage.LocationBlacklist as Storage
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable ::
     B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.LocationBlacklistT)
dbTable = DB._locationBlacklist DB.becknDb

findLocationBlacklist ::
     Text -> L.Flow (T.DBResult (Maybe Storage.LocationBlacklist))
findLocationBlacklist id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.LocationBlacklist {..} = (_id ==. B.val_ id)
