{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Beckn.Storage.Queries where

import qualified Beckn.Storage.DB.Config as DB
import Beckn.Types.Common (FlowR)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified Database.Beam.Query.Internal as BI
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Servant (err500)

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

data DBEnv = DBEnv
  { schemaName :: Text,
    currentTime :: UTCTime
  }

type SqlDB = ReaderT DBEnv (L.SqlDB Pg)

instance HasSchemaName SqlDB where
  getSchemaName = asks schemaName

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
  currentTime <- getCurrTime
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
runSqlDBTransaction = runSqlDB' L.runTransaction >=> either throwDBError pure

run :: (HasCallStack, T.JSONEx a) => L.SqlDB Pg a -> DB.FlowWithDb r (T.DBResult a)
run query = do
  connection <- DB.getOrInitConn
  L.runDB connection query

findOneWithErr ::
  ( HasCallStack,
    RunReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  DB.FlowWithDb r (table Identity)
findOneWithErr dbTable predicate = do
  res <- run $ findOne' dbTable predicate
  case res of
    Right (Just val) -> return val
    Right Nothing -> throwError500 "NO_REC_FOUND"
    Left err -> throwErrorWithInfo500 "DB_ERROR" $ "DBError: " <> show err

findOne ::
  ( HasCallStack,
    RunReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  DB.FlowWithDb r (T.DBResult (Maybe (table Identity)))
findOne dbTable predicate = run $ findOne' dbTable predicate

findOne' ::
  ( HasCallStack,
    ReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg (Maybe (table Identity))
findOne' dbTable predicate =
  L.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

findAllOrErr ::
  ( HasCallStack,
    RunReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  DB.FlowWithDb r [table Identity]
findAllOrErr dbTable predicate = do
  res <- run $ findAll' dbTable predicate
  case res of
    Right val -> return val
    Left err -> throwErrorWithInfo500 "DB_ERROR" $ "DBError: " <> show err

findAll ::
  ( HasCallStack,
    RunReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  DB.FlowWithDb r (T.DBResult [table Identity])
findAll dbTable predicate = run $ findAll' dbTable predicate

findAll' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg [table Identity]
findAll' dbTable predicate =
  L.findRows $ B.select $ B.filter_ predicate $ B.all_ dbTable

-- TODO: protect from multiple inserts
createOne ::
  (HasCallStack, RunPgTable table db) =>
  Table table db -> -- dbTable
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  DB.FlowWithDb r (T.DBResult ())
createOne = bulkInsert

createOne' ::
  (HasCallStack, PgTable table db) =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
createOne' = bulkInsert'

bulkInsert ::
  (HasCallStack, RunPgTable table db) =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  DB.FlowWithDb r (T.DBResult ())
bulkInsert dbTable = run . bulkInsert' dbTable

bulkInsert' ::
  (HasCallStack, PgTable table db) =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
bulkInsert' dbTable =
  L.insertRows . B.insert dbTable

findOrCreateOne ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  DB.FlowWithDb r (T.DBResult (Maybe (table Identity)))
findOrCreateOne dbTable findPredicate insertExpression =
  run $ findOrCreateOne' dbTable findPredicate insertExpression

findOrCreateOne' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg (Maybe (table Identity))
findOrCreateOne' dbTable findPredicate insertExpression =
  findAll' dbTable findPredicate >>= \case
    [] -> do
      createOne' dbTable insertExpression
      findOne' dbTable findPredicate
    (s : xs) ->
      if null xs
        then pure (Just s)
        else error "multiple rows found"

findAllRows ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  DB.FlowWithDb r (T.DBResult [table Identity])
findAllRows dbTable = run $ findAllRows' dbTable

findAllRows' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  L.SqlDB Pg [table Identity]
findAllRows' dbTable = L.findRows $ B.select $ B.all_ dbTable

-- use this only for single sql statement, for multiple use transactions
update ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  DB.FlowWithDb r (T.DBResult ())
update dbTable setClause predicate = runSqlDB $ update' dbTable setClause predicate

update' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  SqlDB ()
update' dbTable setClause predicate = lift . L.updateRows $ B.update dbTable setClause predicate

delete ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  DB.FlowWithDb r (T.DBResult ())
delete dbTable predicate = run $ delete' dbTable predicate

delete' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.SqlDB Pg ()
delete' dbTable predicate = L.deleteRows $ B.delete dbTable predicate

throwDBError ::
  ( HasCallStack,
    L.MonadFlow mFlow,
    Log mFlow
  ) =>
  T.DBError ->
  mFlow a
throwDBError err =
  logError "DB error: " (show err)
    >> L.throwException err500

type Scope2 = BI.QNested (BI.QNested B.QBaseScope)

type Scope3 = BI.QNested (BI.QNested (BI.QNested B.QBaseScope))

findAllWithLimitOffset ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  DB.FlowWithDb r (T.DBResult [table Identity])
findAllWithLimitOffset dbTable limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ B.all_ dbTable

findAllWithLimitOffsetWhere ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  (table (BI.QGenExpr BI.QValueContext Postgres Scope2) -> BI.QExpr Postgres Scope2 Bool) ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  DB.FlowWithDb r (T.DBResult [table Identity])
findAllWithLimitOffsetWhere dbTable predicate limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.filter_ predicate $ B.orderBy_ orderBy $ B.all_ dbTable

aggregate ::
  ( HasCallStack,
    RunReadablePgTable table db,
    _
  ) =>
  Table table db ->
  _aggregator ->
  _predicate ->
  DB.FlowWithDb r (T.DBResult [_result])
aggregate dbTable aggregator predicate = run $ L.findRows $ B.select $ B.aggregate_ aggregator $ B.filter_ predicate $ B.all_ dbTable

findAllByJoin ::
  (HasCallStack, _) =>
  Integer ->
  Integer ->
  _orderBy ->
  _query ->
  DB.FlowWithDb r (T.DBResult [_result])
findAllByJoin limit offset orderBy =
  run . L.findRows . B.select . B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy

findAllByJoinWithoutLimits ::
  (HasCallStack, _) =>
  _orderBy ->
  _query ->
  DB.FlowWithDb r (T.DBResult [_result])
findAllByJoinWithoutLimits orderBy =
  run . L.findRows . B.select . B.orderBy_ orderBy
