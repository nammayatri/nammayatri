{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Location where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.DB as DB
import qualified Beckn.Types.Storage.Location as Storage
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.becknDb

findLocation :: Text -> L.Flow (T.DBResult (Maybe Storage.Location))
findLocation id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.Location {..} = (_id ==. B.val_ id)
