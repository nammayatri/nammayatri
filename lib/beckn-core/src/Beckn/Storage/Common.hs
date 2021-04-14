{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Beckn.Storage.Common
  ( insertExpression,
    prepareDBConnections,
    getOrInitConn,
  )
where

import Beckn.Storage.DB.Config
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import GHC.Records (HasField (..))

insertExpression ::
  _ =>
  table B.Identity ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s))
insertExpression value = B.insertValues [value]

handleIt ::
  (T.DBConfig Pg -> FlowR r (T.DBResult (T.SqlConn Pg))) ->
  FlowWithDb r (T.DBResult (T.SqlConn Pg))
handleIt mf = do
  env <- ask
  mf . repack $ getField @"dbCfg" env
  where
    repack (DBConfig x y z _) = T.mkPostgresPoolConfig x y z

prepareDBConnections :: FlowWithDb r (T.DBResult (T.SqlConn Pg))
prepareDBConnections = handleIt L.initSqlDBConnection

getOrInitConn :: FlowWithDb r (T.SqlConn Pg)
getOrInitConn = handleIt L.getOrInitSqlConn >>= either (throwErrorWithInfo DBUnknownError . show) pure
