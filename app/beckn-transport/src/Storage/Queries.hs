module Storage.Queries where

import App.Types
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified Database.Beam.Query.Internal as BI
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Servant (err500, errBody)
import qualified Storage.DB.Config as DB
import Types.Config (Config (..))

type PgTable table db =
  ( B.Beamable table,
    B.Database Postgres db
  )

type RunPgTable table db =
  ( PgTable table db,
    Config T.PostgresConfig
  )

type ReadablePgTable table db =
  ( PgTable table db,
    B.FromBackendRow Postgres (table Identity)
  )

type RunReadablePgTable table db =
  ( ReadablePgTable table db,
    T.JSONEx (table Identity),
    Config T.PostgresConfig
  )

run ::
  ( T.JSONEx a,
    Config T.PostgresConfig
  ) =>
  L.SqlDB Pg a ->
  Flow (T.DBResult a)
run query = do
  sqlConfig <- DB.getPgDBConfig theConfig
  connection <- DB.getOrInitConn sqlConfig
  L.runDB connection query

-- find queries
findOneWithErr ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  Flow (table Identity)
findOneWithErr dbTable predicate = do
  res <- run $ findOne' dbTable predicate
  case res of
    Right (Just val) -> return val
    Right Nothing -> L.throwException err500 {errBody = "No rec found"}
    Left err -> L.throwException err500 {errBody = "DBError: " <> show err}

findOne ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  Flow (T.DBResult (Maybe (table Identity)))
findOne dbTable predicate = run $ findOne' dbTable predicate

findOne' ::
  ReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg (Maybe (table Identity))
findOne' dbTable predicate =
  L.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

findAllOrErr ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  Flow [table Identity]
findAllOrErr dbTable predicate = do
  res <- run $ findAll' dbTable predicate
  case res of
    Right val -> return val
    Left err -> L.throwException err500 {errBody = "DBError: " <> show err}

findAll ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  Flow (T.DBResult [table Identity])
findAll dbTable predicate = run $ findAll' dbTable predicate

findAll' ::
  ReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg [table Identity]
findAll' dbTable predicate =
  L.findRows $ B.select $ B.filter_ predicate $ B.all_ dbTable

-- TODO: protect from multiple inserts
createOne ::
  RunPgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) -> -- dbTable
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  Flow (T.DBResult ())
createOne = bulkInsert

createOne' ::
  PgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
createOne' = bulkInsert'

bulkInsert ::
  RunPgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  Flow (T.DBResult ())
bulkInsert dbTable = run . bulkInsert' dbTable

bulkInsert' ::
  PgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
bulkInsert' dbTable =
  L.insertRows . B.insert dbTable

findOrCreateOne ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  Flow (T.DBResult (Maybe (table Identity)))
findOrCreateOne dbTable findPredicate insertExpression =
  run $ findOrCreateOne' dbTable findPredicate insertExpression

findOrCreateOne' ::
  ReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
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
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  Flow (T.DBResult [table Identity])
findAllRows dbTable = run $ findAllRows' dbTable

findAllRows' ::
  ReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  L.SqlDB Pg [table Identity]
findAllRows' dbTable = L.findRows $ B.select $ B.all_ dbTable

update ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  Flow (T.DBResult ())
update dbTable setClause predicate = run $ update' dbTable setClause predicate

update' ::
  ReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.SqlDB Pg ()
update' dbTable setClause predicate = L.updateRows $ B.update dbTable setClause predicate

delete ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  Flow (T.DBResult ())
delete dbTable predicate = run $ delete' dbTable predicate

delete' ::
  ReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.SqlDB Pg ()
delete' dbTable predicate = L.deleteRows $ B.delete dbTable predicate

throwDBError :: T.DBError -> Flow a
throwDBError err =
  L.logError "DB error: " (show err)
    >> L.throwException err500

type Scope3 = BI.QNested (BI.QNested (BI.QNested B.QBaseScope))

findAllWithLimitOffset ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  Flow (T.DBResult [table Identity])
findAllWithLimitOffset dbTable limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ B.all_ dbTable

--findAllWithLimitOffsetWhere ::
--RunReadablePgTable table db
-- => B.DatabaseEntity .Postgres db (B.TableEntity table)
-- -> (table (B.QExpr .Postgres B.QBaseScope) -> B.QExpr .Postgres B.QBaseScope Bool)
-- -> Integer
-- -> Integer
-- -> (table (B.QExpr .Postgres Scope3) -> BI.QOrd .Postgres Scope3 ordering)
-- -> Flow (T.DBResult [table Identity])
findAllWithLimitOffsetWhere dbTable predicate limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.filter_ predicate $ B.orderBy_ orderBy $ B.all_ dbTable

aggregate dbTable aggregator predicate = run $ L.findRows $ B.select $ B.aggregate_ aggregator $ B.filter_ predicate $ B.all_ dbTable

findAllByJoin limit offset orderBy =
  run . L.findRows . B.select . B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy

findAllByJoinWithoutLimits orderBy =
  run . L.findRows . B.select . B.orderBy_ orderBy
