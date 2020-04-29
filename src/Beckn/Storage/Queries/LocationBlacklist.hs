{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.LocationBlacklist where

import           Database.Beam                         ((&&.), (<-.), (==.))
import           EulerHS.Prelude                       hiding (id)

import qualified Beckn.Storage.Queries                 as DB
import qualified Beckn.Storage.Queries                 as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB                as DB
import qualified Beckn.Types.Storage.DB                as DB
import qualified Beckn.Types.Storage.LocationBlacklist as Storage
import           Beckn.Utils.Common
import           Data.Time
import           Data.Time.LocalTime
import qualified Database.Beam                         as B
import qualified Database.Beam                         as B
import qualified EulerHS.Language                      as L
import qualified EulerHS.Language                      as L
import qualified EulerHS.Types                         as T
import qualified EulerHS.Types                         as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.LocationBlacklistT)
dbTable = DB._locationBlacklist DB.becknDb

create :: Storage.LocationBlacklist -> L.Flow ()
create Storage.LocationBlacklist {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.LocationBlacklist {..}) >>=
  either DB.throwDBError pure

findById :: LocationBlacklistId -> L.Flow (T.DBResult (Maybe Storage.LocationBlacklist))
findById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.LocationBlacklist {..} = (_id ==. B.val_ id)
