{-# LANGUAGE TypeApplications #-}

module Beckn.Storage.Queries.ExternalTrail where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Config as DB
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Storage.ExternalTrail as Storage
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import GHC.Records (HasField (..))

getDbTable :: DB.FlowWithDb r (B.DatabaseEntity be DB.TrailDb (B.TableEntity Storage.ExternalTrailT))
getDbTable = do
  dbCfg <- getField @"dbCfg" <$> ask
  pure $ DB._externalTrail (DB.trailDb $ schemaName $ DB.pgConfig dbCfg)
  where
    schemaName T.PostgresConfig {..} = fromString connectDatabase

create :: Storage.ExternalTrail -> DB.FlowWithDb r (T.DBResult ())
create session = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression session)
