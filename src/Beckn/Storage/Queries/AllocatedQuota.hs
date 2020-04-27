{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.AllocatedQuota where

import Database.Beam ((&&.), (<-.), (==.))
import EulerHS.Prelude hiding (id)

import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.AllocatedQuota as Storage
import qualified Beckn.Types.Storage.DB as DB
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

dbTable ::
     B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.AllocatedQuotaT)
dbTable = DB._allocatedQuota DB.becknDb

findAllocatedQuota :: Text -> L.Flow (T.DBResult (Maybe Storage.AllocatedQuota))
findAllocatedQuota id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.AllocatedQuota {..} = (_id ==. B.val_ id)
