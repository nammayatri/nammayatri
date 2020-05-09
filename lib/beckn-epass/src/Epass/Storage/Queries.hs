module Epass.Storage.Queries where

import           Epass.Types.Config           (Config (..))

import           EulerHS.Prelude              hiding (id)

import qualified Epass.Storage.DB.Config      as DB
import qualified Database.Beam                as B
import qualified Database.Beam.MySQL          as BM
import qualified Database.Beam.Query.Internal as BI
import qualified EulerHS.Language             as L
import qualified EulerHS.Types                as T
import           Servant                      (err500, errBody)

type MySqlTable table db =
    ( B.Beamable table
    , B.Database BM.MySQL db
    )

type RunMySqlTable table db =
    ( MySqlTable table db
    , Config T.MySQLConfig
    )

type ReadableMySqlTable table db =
    ( MySqlTable table db
    , B.FromBackendRow BM.MySQL (table Identity)
    )

type RunReadableMySqlTable table db =
    ( ReadableMySqlTable table db
    , T.JSONEx (table Identity)
    , Config T.MySQLConfig
    )

run ::
    ( T.JSONEx a
    , Config T.MySQLConfig
    )
  => L.SqlDB BM.MySQLM a -> L.Flow (T.DBResult a)
run query = do
  sqlConfig <- DB.getMysqlDBConfig theConfig
  connection <- DB.getOrInitConn sqlConfig
  L.runDB connection query

-- find queries
findOneWithErr ::
    RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> L.Flow (table Identity)
findOneWithErr dbTable predicate = do
  res <- run $ findOne' dbTable predicate
  case res of
    Right (Just val) -> return val
    Right Nothing    -> L.throwException err500 {errBody = "No rec found"}
    Left  err        -> L.throwException err500 {errBody = ("DBError: " <> show err)}

findOne ::
    RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> L.Flow (T.DBResult (Maybe (table Identity)))
findOne dbTable predicate = run $ findOne' dbTable predicate

findOne' ::
     ReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> L.SqlDB BM.MySQLM (Maybe (table Identity))
findOne' dbTable predicate =
  L.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

findAllOrErr ::
    RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> L.Flow [table Identity]
findAllOrErr dbTable predicate = do
  res <- run $ findAll' dbTable predicate
  case res of
    Right val -> return val
    Left err  -> L.throwException err500 {errBody = ("DBError: " <> show err)}

findAll ::
    RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> L.Flow (T.DBResult ([table Identity]))
findAll dbTable predicate = run $ findAll' dbTable predicate

findAll' ::
    ReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> L.SqlDB BM.MySQLM [table Identity]
findAll' dbTable predicate =
  L.findRows $ B.select $ B.filter_ predicate $ B.all_ dbTable

-- TODO: protect from multiple inserts
createOne ::
    RunMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table) -- dbTable
  -> B.SqlInsertValues BM.MySQL (table (B.QExpr BM.MySQL s))
  -> L.Flow (T.DBResult ())
createOne dbTable insertExpression = bulkInsert dbTable insertExpression

createOne' ::
    MySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> B.SqlInsertValues BM.MySQL (table (B.QExpr BM.MySQL s))
  -> L.SqlDB BM.MySQLM ()
createOne' dbTable insertExpression = bulkInsert' dbTable insertExpression

bulkInsert ::
    RunMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> B.SqlInsertValues BM.MySQL (table (B.QExpr BM.MySQL s))
  -> L.Flow (T.DBResult ())
bulkInsert dbTable insertExpressions = run $ bulkInsert' dbTable insertExpressions

bulkInsert' ::
    MySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> B.SqlInsertValues BM.MySQL (table (B.QExpr BM.MySQL s))
  -> L.SqlDB BM.MySQLM ()
bulkInsert' dbTable insertExpressions =
  L.insertRows $ B.insert dbTable $ insertExpressions

findOrCreateOne ::
    RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> B.SqlInsertValues BM.MySQL (table (B.QExpr BM.MySQL s))
  -> L.Flow (T.DBResult (Maybe (table Identity)))
findOrCreateOne dbTable findPredicate insertExpression =
  run $ findOrCreateOne' dbTable findPredicate insertExpression

findOrCreateOne' ::
    ReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -> B.SqlInsertValues BM.MySQL (table (B.QExpr BM.MySQL s))
  -> L.SqlDB BM.MySQLM (Maybe (table Identity))
findOrCreateOne' dbTable findPredicate insertExpression =
  findAll' dbTable findPredicate >>= \case
    [] -> do
      createOne' dbTable insertExpression
      findOne' dbTable findPredicate
    (s:xs) ->
      if null xs
        then pure (Just s)
        else error "multiple rows found"

findAllRows ::
    RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> L.Flow (T.DBResult ([table Identity]))
findAllRows dbTable = run $ findAllRows' dbTable

findAllRows' ::
    ReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> L.SqlDB BM.MySQLM [table Identity]
findAllRows' dbTable = L.findRows $ B.select $ B.all_ dbTable

update ::
    RunReadableMySqlTable table db
  => (B.DatabaseEntity BM.MySQL db (B.TableEntity table))
  -> (forall s. table (B.QField s) -> B.QAssignment BM.MySQL s)
  -> (forall s. table (B.QExpr BM.MySQL s) -> B.QExpr BM.MySQL s Bool)
  -> L.Flow (T.DBResult ())
update dbTable setClause predicate = run $ update' dbTable setClause predicate

update' ::
    ReadableMySqlTable table db
  => (B.DatabaseEntity BM.MySQL db (B.TableEntity table))
  -> (forall s. table (B.QField s) -> B.QAssignment BM.MySQL s)
  -> (forall s. table (B.QExpr BM.MySQL s) -> B.QExpr BM.MySQL s Bool)
  -> L.SqlDB BM.MySQLM ()
update' dbTable setClause predicate = L.updateRows $ B.update dbTable setClause predicate

delete
  :: RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (forall s. table (B.QExpr BM.MySQL s) -> B.QExpr BM.MySQL s Bool)
  -> F L.FlowMethod (T.DBResult ())
delete dbTable predicate = run $ delete' dbTable predicate

delete'
  :: ReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> (forall s. table (B.QExpr BM.MySQL s) -> B.QExpr BM.MySQL s Bool)
  -> L.SqlDB BM.MySQLM ()
delete' dbTable predicate = L.deleteRows $ B.delete dbTable predicate

throwDBError :: T.DBError -> L.Flow a
throwDBError err =
  L.logError "DB error: " (show err) >>
  L.throwException err500

type Scope3 = BI.QNested (BI.QNested (BI.QNested B.QBaseScope))

findAllWithLimitOffset ::
    RunReadableMySqlTable table db
  => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -> Integer
  -> Integer
  -> (table (B.QExpr BM.MySQL Scope3) -> BI.QOrd BM.MySQL Scope3 ordering)
  -> L.Flow (T.DBResult [table Identity])
findAllWithLimitOffset dbTable limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ B.all_ dbTable

--findAllWithLimitOffsetWhere ::
    --RunReadableMySqlTable table db
  -- => B.DatabaseEntity BM.MySQL db (B.TableEntity table)
  -- -> (table (B.QExpr BM.MySQL B.QBaseScope) -> B.QExpr BM.MySQL B.QBaseScope Bool)
  -- -> Integer
  -- -> Integer
  -- -> (table (B.QExpr BM.MySQL Scope3) -> BI.QOrd BM.MySQL Scope3 ordering)
  -- -> L.Flow (T.DBResult [table Identity])
findAllWithLimitOffsetWhere dbTable predicate limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.filter_ predicate $ B.orderBy_ orderBy $ B.all_ dbTable
