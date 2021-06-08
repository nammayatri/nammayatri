{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Beckn.Storage.Queries where

import qualified Beckn.Storage.Common as DB
import Beckn.Storage.DB.Config hiding (schemaName)
import qualified Beckn.Storage.DB.Config as DB
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified Database.Beam.Query.Internal as BI
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

data DBEnv = DBEnv
  { schemaName :: Text,
    currentTime :: UTCTime
  }

type SqlDB = ReaderT DBEnv (L.SqlDB Pg)

instance HasSchemaName SqlDB where
  getSchemaName = asks schemaName

instance MonadTime SqlDB where
  getCurrentTime = asks currentTime

findOne ::
  ( HasCallStack,
    ReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  FlowWithDb r (Maybe (table Identity))
findOne dbTable predicate = runSqlDB $ lift . L.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

findAll ::
  ( HasCallStack,
    ReadablePgTable table db,
    B.FromBackendRow Postgres (B.QExprToIdentity res),
    BI.ProjectibleWithPredicate
      BI.AnyType
      Postgres
      (BI.WithExprContext (BI.BeamSqlBackendExpressionSyntax' Postgres))
      res
  ) =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  ( BI.Q Postgres db s (table (BI.QExpr Postgres s)) ->
    BI.Q Postgres db B.QBaseScope res
  ) ->
  (table (BI.QExpr Postgres s) -> BI.QExpr Postgres s Bool) ->
  FlowWithDb r [B.QExprToIdentity res]
findAll dbTable commands predicate = runSqlDB $ lift . L.findRows . B.select . commands . B.filter_ predicate $ B.all_ dbTable

update ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QField s) -> B.QAssignment Postgres s) ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  FlowWithDb r ()
update dbTable setClause predicate = runSqlDB $ update' dbTable setClause predicate

createOne ::
  (HasCallStack, PgTable table db) =>
  Table table db ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s)) ->
  FlowWithDb r ()
createOne dbTable value = runSqlDB $ createOne' dbTable value

delete ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  FlowWithDb r ()
delete dbTable predicate = runSqlDB $ delete' dbTable predicate

deleteReturning ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  FlowWithDb r [table Identity]
deleteReturning dbTable predicate = runSqlDB $ deleteReturning' dbTable predicate

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

deleteReturning' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  (forall s. table (B.QExpr Postgres s) -> B.QExpr Postgres s Bool) ->
  SqlDB [table Identity]
deleteReturning' dbTable predicate = lift . L.deleteRowsReturningListPG $ B.delete dbTable predicate

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

findAllByJoin ::
  ( HasCallStack,
    B.FromBackendRow Postgres (B.QExprToIdentity res),
    BI.ProjectibleWithPredicate
      BI.AnyType
      Postgres
      (BI.WithExprContext (BI.BeamSqlBackendExpressionSyntax' Postgres))
      res
  ) =>
  (_query -> BI.Q Postgres db B.QBaseScope res) ->
  _query ->
  DB.FlowWithDb r [B.QExprToIdentity res]
findAllByJoin filters query =
  runSqlDB . lift . L.findRows . B.select $ filters query
