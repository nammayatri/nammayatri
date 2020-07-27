{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Beckn.Storage.Queries where

import qualified Beckn.Storage.DB.Config as DB
import Beckn.Types.App
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified Database.Beam.Query.Internal as BI
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Servant (err500, errBody)

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

run ::
  ( L.MonadFlow mFlow,
    HasCommonEnv mFlow,
    T.JSONEx a
  ) =>
  L.SqlDB Pg a ->
  mFlow (T.DBResult a)
run query = do
  sqlConfig <- DB.getPgDBConfig
  connection <- DB.getOrInitConn sqlConfig
  L.runDB connection query

-- find queries
findOneWithErr ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  mFlow (table Identity)
findOneWithErr dbTable predicate = do
  res <- run $ findOne' dbTable predicate
  case res of
    Right (Just val) -> return val
    Right Nothing -> L.throwException err500 {errBody = "No rec found"}
    Left err -> L.throwException err500 {errBody = "DBError: " <> show err}

findOne ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  mFlow (T.DBResult (Maybe (table Identity)))
findOne dbTable predicate = run $ findOne' dbTable predicate

findOne' ::
  ReadablePgTable table db =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg (Maybe (table Identity))
findOne' dbTable predicate =
  L.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

findAllOrErr ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  mFlow [table Identity]
findAllOrErr dbTable predicate = do
  res <- run $ findAll' dbTable predicate
  case res of
    Right val -> return val
    Left err -> L.throwException err500 {errBody = "DBError: " <> show err}

findAll ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  mFlow (T.DBResult [table Identity])
findAll dbTable predicate = run $ findAll' dbTable predicate

findAll' ::
  ReadablePgTable table db =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg [table Identity]
findAll' dbTable predicate =
  L.findRows $ B.select $ B.filter_ predicate $ B.all_ dbTable

-- TODO: protect from multiple inserts
createOne ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunPgTable table db) =>
  Table table db -> -- dbTable
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  mFlow (T.DBResult ())
createOne = bulkInsert

createOne' ::
  PgTable table db =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
createOne' = bulkInsert'

bulkInsert ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunPgTable table db) =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  mFlow (T.DBResult ())
bulkInsert dbTable = run . bulkInsert' dbTable

bulkInsert' ::
  PgTable table db =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
bulkInsert' dbTable =
  L.insertRows . B.insert dbTable

findOrCreateOne ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  mFlow (T.DBResult (Maybe (table Identity)))
findOrCreateOne dbTable findPredicate insertExpression =
  run $ findOrCreateOne' dbTable findPredicate insertExpression

findOrCreateOne' ::
  ReadablePgTable table db =>
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
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  mFlow (T.DBResult [table Identity])
findAllRows dbTable = run $ findAllRows' dbTable

findAllRows' ::
  ReadablePgTable table db =>
  Table table db ->
  L.SqlDB Pg [table Identity]
findAllRows' dbTable = L.findRows $ B.select $ B.all_ dbTable

update ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  mFlow (T.DBResult ())
update dbTable setClause predicate = run $ update' dbTable setClause predicate

update' ::
  ReadablePgTable table db =>
  Table table db ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.SqlDB Pg ()
update' dbTable setClause predicate = L.updateRows $ B.update dbTable setClause predicate

delete ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  mFlow (T.DBResult ())
delete dbTable predicate = run $ delete' dbTable predicate

delete' ::
  ReadablePgTable table db =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.SqlDB Pg ()
delete' dbTable predicate = L.deleteRows $ B.delete dbTable predicate

throwDBError :: L.MonadFlow mFlow => T.DBError -> mFlow a
throwDBError err =
  L.logError "DB error: " (show err)
    >> L.throwException err500

type Scope2 = BI.QNested (BI.QNested B.QBaseScope)

type Scope3 = BI.QNested (BI.QNested (BI.QNested B.QBaseScope))

findAllWithLimitOffset ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow, RunReadablePgTable table db) =>
  Table table db ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  mFlow (T.DBResult [table Identity])
findAllWithLimitOffset dbTable limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ B.all_ dbTable

findAllWithLimitOffsetWhere ::
  ( L.MonadFlow mFlow,
    HasCommonEnv mFlow,
    RunReadablePgTable table db
  ) =>
  Table table db ->
  (table (BI.QGenExpr BI.QValueContext Postgres Scope2) -> BI.QExpr Postgres Scope2 Bool) ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  mFlow (T.DBResult [table Identity])
findAllWithLimitOffsetWhere dbTable predicate limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.filter_ predicate $ B.orderBy_ orderBy $ B.all_ dbTable

aggregate ::
  ( L.MonadFlow mFlow,
    HasCommonEnv mFlow,
    RunReadablePgTable table db,
    _
  ) =>
  Table table db ->
  _aggregator ->
  _predicate ->
  mFlow (T.DBResult [_result])
aggregate dbTable aggregator predicate = run $ L.findRows $ B.select $ B.aggregate_ aggregator $ B.filter_ predicate $ B.all_ dbTable

findAllByJoin ::
  ( L.MonadFlow mFlow,
    HasCommonEnv mFlow,
    _
  ) =>
  Integer ->
  Integer ->
  _orderBy ->
  _query ->
  mFlow (T.DBResult [_result])
findAllByJoin limit offset orderBy =
  run . L.findRows . B.select . B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy

findAllByJoinWithoutLimits ::
  ( L.MonadFlow mFlow,
    HasCommonEnv mFlow,
    _
  ) =>
  _orderBy ->
  _query ->
  mFlow (T.DBResult [_result])
findAllByJoinWithoutLimits orderBy =
  run . L.findRows . B.select . B.orderBy_ orderBy
