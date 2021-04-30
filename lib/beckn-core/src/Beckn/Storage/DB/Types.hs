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
  findOne ::
    ( HasCallStack,
      ReadablePgTable table db
    ) =>
    Table table db ->
    (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
    m (Maybe (table Identity))
  findAll ::
    (HasCallStack, ReadablePgTable table db) =>
    Table table db ->
    (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
    m [table Identity]
  update ::
    (HasCallStack, RunReadablePgTable table db) =>
    Table table db ->
    (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
    (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
    m ()
  createOne ::
    (HasCallStack, PgTable table db) =>
    Table table db ->
    B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
    m ()
  delete ::
    (HasCallStack, RunReadablePgTable table db) =>
    Table table db ->
    (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
    m ()

data DBEnv = DBEnv
  { schemaName :: Text,
    currentTime :: UTCTime
  }

type SqlDB = ReaderT DBEnv (L.SqlDB Pg)

instance HasSchemaName SqlDB where
  getSchemaName = asks schemaName

instance DBUser SqlDB where
  findOne = findOne'
  findAll = findAll'
  update = update'
  delete = delete'
  createOne = createOne'

findOne' ::
  ( HasCallStack,
    ReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  SqlDB (Maybe (table Identity))
findOne' dbTable predicate = lift . L.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

findAll' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  SqlDB [table Identity]
findAll' dbTable predicate = lift . L.findRows $ B.select $ B.filter_ predicate $ B.all_ dbTable

update' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  SqlDB ()
update' dbTable setClause predicate = lift $ L.updateRows (B.update dbTable setClause predicate)

createOne' ::
  (HasCallStack, PgTable table db) =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  SqlDB ()
createOne' dbTable value = lift . L.insertRows $ B.insert dbTable value

delete' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  SqlDB ()
delete' dbTable predicate = lift . L.deleteRows $ B.delete dbTable predicate

instance (DB.HasDbCfg r) => DBUser (FlowR r) where
  findOne dbTable predicate = runSqlDB (findOne' dbTable predicate)
  findAll dbTable predicate = runSqlDB (findAll' dbTable predicate)
  update dbTable setClause predicate = runSqlDB (update' dbTable setClause predicate)
  delete dbTable predicate = runSqlDB (delete' dbTable predicate)
  createOne dbTable value = runSqlDB (createOne' dbTable value)

runSqlDB' ::
  HasCallStack =>
  ( T.SqlConn Pg ->
    L.SqlDB Pg a ->
    FlowR r (T.DBResult a)
  ) ->
  SqlDB a ->
  DB.FlowWithDb r a
runSqlDB' runSqlDBFunction query = do
  connection <- DB.getOrInitConn
  schemaName <- getSchemaName
  currentTime <- getCurrentTime
  let env = DBEnv {..}
  runSqlDBFunction connection (runReaderT query env) >>= checkDBError

runSqlDB ::
  HasCallStack =>
  SqlDB a ->
  DB.FlowWithDb r a
runSqlDB = runSqlDB' L.runDB

runSqlDBTransaction ::
  HasCallStack =>
  SqlDB a ->
  DB.FlowWithDb r a
runSqlDBTransaction = runSqlDB' L.runTransaction
