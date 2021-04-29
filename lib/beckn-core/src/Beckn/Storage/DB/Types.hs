{-# LANGUAGE UndecidableInstances #-}

module Beckn.Storage.DB.Types where

import qualified Beckn.Storage.Common as DB
import qualified Beckn.Storage.DB.Config as DB
import Beckn.Types.Flow
import Beckn.Types.Schema
import Beckn.Types.Time
import Beckn.Utils.Dhall
import Beckn.Utils.Error.Throwing
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T

type PgTable table db =
  ( B.Beamable table,
    B.Database Postgres db
  )

type RunPgTable table db =
  PgTable table db

type ReadablePgTable table db =
  ( PgTable table db,
    B.FromBackendRow Postgres (table Identity)
  )

type RunReadablePgTable table db =
  ( ReadablePgTable table db,
    T.JSONEx (table Identity)
  )

type Table table db = B.DatabaseEntity Postgres db (B.TableEntity table)

class DBUser m where
  update ::
    (HasCallStack, RunReadablePgTable table db) =>
    Table table db ->
    (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
    (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
    m (Either T.DBError ())
  createOne ::
    (HasCallStack, PgTable table db) =>
    Table table db ->
    B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
    m (Either T.DBError ())
  delete ::
    (HasCallStack, RunReadablePgTable table db) =>
    Table table db ->
    (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
    m (Either T.DBError ())

data DBEnv = DBEnv
  { schemaName :: Text,
    currentTime :: UTCTime
  }

type SqlDB = ReaderT DBEnv (L.SqlDB Pg)

instance HasSchemaName SqlDB where
  getSchemaName = asks schemaName

instance DBUser SqlDB where
  update = update'
  delete = delete'
  createOne = createOne'

update' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  SqlDB (Either T.DBError ())
update' dbTable setClause predicate = do
  lift $ L.updateRows (B.update dbTable setClause predicate)
  return $ Right ()

createOne' ::
  (HasCallStack, PgTable table db) =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  SqlDB (Either T.DBError ())
createOne' dbTable value = do
  lift . L.insertRows $ B.insert dbTable value
  return $ Right ()

delete' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  SqlDB (Either T.DBError ())
delete' dbTable predicate = do
  lift . L.deleteRows $ B.delete dbTable predicate
  return $ Right ()

instance (DB.HasDbCfg r) => DBUser (FlowR r) where
  update dbTable setClause predicate = runSqlDB (update' dbTable setClause predicate) >>= checkDBError
  delete dbTable predicate = runSqlDB (delete' dbTable predicate) >>= checkDBError
  createOne dbTable value = runSqlDB (createOne' dbTable value) >>= checkDBError

runSqlDB' ::
  (HasCallStack, T.JSONEx a) =>
  ( T.SqlConn Pg ->
    L.SqlDB Pg a ->
    FlowR r (T.DBResult a)
  ) ->
  SqlDB a ->
  DB.FlowWithDb r (T.DBResult a)
runSqlDB' runSqlDBFunction query = do
  connection <- DB.getOrInitConn
  schemaName <- getSchemaName
  currentTime <- getCurrentTime
  let env = DBEnv {..}
  runSqlDBFunction connection (runReaderT query env)

runSqlDB ::
  (HasCallStack, T.JSONEx a) =>
  SqlDB a ->
  DB.FlowWithDb r (T.DBResult a)
runSqlDB = runSqlDB' L.runDB

runSqlDBTransaction ::
  (HasCallStack, T.JSONEx a) =>
  SqlDB a ->
  DB.FlowWithDb r a
runSqlDBTransaction = runSqlDB' L.runTransaction >=> checkDBError
