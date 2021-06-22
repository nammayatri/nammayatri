{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Beckn.Storage.Common
  ( insertExpression,
    prepareDBConnections,
    getOrInitConn,
  )
where

import Beckn.Storage.DB.Config
import Beckn.Utils.Common
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T

insertExpression ::
  _ =>
  table B.Identity ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s))
insertExpression value = B.insertValues [value]

handleIt ::
  HasFlowDBEnv m r =>
  (T.DBConfig Pg -> m (T.DBResult (T.SqlConn Pg))) ->
  m (T.DBResult (T.SqlConn Pg))
handleIt mf = do
  cfg <- asks (.dbCfg)
  mf $ repack cfg
  where
    repack (DBConfig x y z _) = T.mkPostgresPoolConfig x y z

prepareDBConnections :: HasFlowDBEnv m r => m (T.DBResult (T.SqlConn Pg))
prepareDBConnections = handleIt L.initSqlDBConnection

getOrInitConn :: HasFlowDBEnv m r => m (T.SqlConn Pg)
getOrInitConn = handleIt L.getOrInitSqlConn >>= checkDBError
