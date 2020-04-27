{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Quota where

import           Database.Beam             ((&&.), (<-.), (==.))
import           EulerHS.Prelude           hiding (id)

import qualified Beckn.Storage.Queries     as DB
import qualified Beckn.Types.Domain.Quota  as Domain
import qualified Beckn.Types.Storage.DB    as DB
import qualified Beckn.Types.Storage.Quota as Storage
import qualified Database.Beam             as B
import qualified EulerHS.Language          as L
import qualified EulerHS.Types             as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.QuotaT)
dbTable = DB._quota DB.becknDb

findQuota :: Text -> L.Flow (T.DBResult (Maybe Domain.Quota))
findQuota id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.Quota {..} = (_id ==. B.val_ id)
