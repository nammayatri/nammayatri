module Storage.Queries where

import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified Database.Beam.Query.Internal as BI
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Servant (err500, errBody)
import qualified Storage.DB.Config as DB
import Types.Config (Config (..))

type MySqlTable table db =
  ( B.Beamable table,
    B.Database Postgres db
  )

type RunMySqlTable table db =
  ( MySqlTable table db,
    Config T.PostgresConfig
  )

type ReadableMySqlTable table db =
  ( MySqlTable table db,
    B.FromBackendRow Postgres (table Identity)
  )

type RunReadableMySqlTable table db =
  ( ReadableMySqlTable table db,
    T.JSONEx (table Identity),
    Config T.PostgresConfig
  )

run ::
  ( T.JSONEx a,
    Config T.PostgresConfig
  ) =>
  L.SqlDB Pg a ->
  L.Flow (T.DBResult a)
run query = do
  sqlConfig <- DB.getPgDBConfig theConfig
  connection <- DB.getOrInitConn sqlConfig
  L.runDB connection query

-- find queries
findOneWithErr ::
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.Flow (table Identity)
findOneWithErr dbTable predicate = do
  res <- run $ findOne' dbTable predicate
  case res of
    Right (Just val) -> return val
    Right Nothing -> L.throwException err500 {errBody = "No rec found"}
    Left err -> L.throwException err500 {errBody = ("DBError: " <> show err)}

findOne ::
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.Flow (T.DBResult (Maybe (table Identity)))
findOne dbTable predicate = run $ findOne' dbTable predicate

findOne' ::
  ReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg (Maybe (table Identity))
findOne' dbTable predicate =
  L.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

findAllOrErr ::
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.Flow [table Identity]
findAllOrErr dbTable predicate = do
  res <- run $ findAll' dbTable predicate
  case res of
    Right val -> return val
    Left err -> L.throwException err500 {errBody = ("DBError: " <> show err)}

findAll ::
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.Flow (T.DBResult ([table Identity]))
findAll dbTable predicate = run $ findAll' dbTable predicate

findAll' ::
  ReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  L.SqlDB Pg [table Identity]
findAll' dbTable predicate =
  L.findRows $ B.select $ B.filter_ predicate $ B.all_ dbTable

-- TODO: protect from multiple inserts
createOne ::
  RunMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) -> -- dbTable
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.Flow (T.DBResult ())
createOne dbTable insertExpression = bulkInsert dbTable insertExpression

createOne' ::
  MySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
createOne' dbTable insertExpression = bulkInsert' dbTable insertExpression

bulkInsert ::
  RunMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.Flow (T.DBResult ())
bulkInsert dbTable insertExpressions = run $ bulkInsert' dbTable insertExpressions

bulkInsert' ::
  MySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.SqlDB Pg ()
bulkInsert' dbTable insertExpressions =
  L.insertRows $ B.insert dbTable $ insertExpressions

findOrCreateOne ::
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  L.Flow (T.DBResult (Maybe (table Identity)))
findOrCreateOne dbTable findPredicate insertExpression =
  run $ findOrCreateOne' dbTable findPredicate insertExpression

findOrCreateOne' ::
  ReadableMySqlTable table db =>
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
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  L.Flow (T.DBResult ([table Identity]))
findAllRows dbTable = run $ findAllRows' dbTable

findAllRows' ::
  ReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  L.SqlDB Pg [table Identity]
findAllRows' dbTable = L.findRows $ B.select $ B.all_ dbTable

update ::
  RunReadableMySqlTable table db =>
  (B.DatabaseEntity Postgres db (B.TableEntity table)) ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.Flow (T.DBResult ())
update dbTable setClause predicate = run $ update' dbTable setClause predicate

update' ::
  ReadableMySqlTable table db =>
  (B.DatabaseEntity Postgres db (B.TableEntity table)) ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.SqlDB Pg ()
update' dbTable setClause predicate = L.updateRows $ B.update dbTable setClause predicate

delete ::
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  F L.FlowMethod (T.DBResult ())
delete dbTable predicate = run $ delete' dbTable predicate

delete' ::
  ReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  L.SqlDB Pg ()
delete' dbTable predicate = L.deleteRows $ B.delete dbTable predicate

throwDBError :: T.DBError -> L.Flow a
throwDBError err =
  L.logError "DB error: " (show err)
    >> L.throwException err500

type Scope3 = BI.QNested (BI.QNested (BI.QNested B.QBaseScope))

findAllWithLimitOffset ::
  RunReadableMySqlTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  L.Flow (T.DBResult [table Identity])
findAllWithLimitOffset dbTable limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ B.all_ dbTable

--findAllWithLimitOffsetWhere ::
--RunReadableMySqlTable table db
-- => B.DatabaseEntity .Postgres db (B.TableEntity table)
-- -> (table (B.QExpr .Postgres B.QBaseScope) -> B.QExpr .Postgres B.QBaseScope Bool)
-- -> Integer
-- -> Integer
-- -> (table (B.QExpr .Postgres Scope3) -> BI.QOrd .Postgres Scope3 ordering)
-- -> L.Flow (T.DBResult [table Identity])
findAllWithLimitOffsetWhere dbTable predicate limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.filter_ predicate $ B.orderBy_ orderBy $ B.all_ dbTable

aggregate dbTable aggregator predicate = run $ L.findRows $ B.select $ B.aggregate_ aggregator $ B.filter_ predicate $ B.all_ dbTable

findAllByJoin limit offset orderBy joinQuery =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ joinQuery

findAllByJoinWithoutLimits orderBy joinQuery =
  run $ L.findRows $ B.select $ B.orderBy_ orderBy $ joinQuery
