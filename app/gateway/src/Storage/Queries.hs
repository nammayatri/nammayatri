{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Storage.Queries where

import App.Types
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import Servant (err500)
import qualified Storage.DB.Config as DB

type PgTable table db =
  ( B.Beamable table,
    B.Database Postgres db
  )

type RunPgTable table db =
  ( PgTable table db,
    DB.Config ET.PostgresConfig
  )

type ReadablePgTable table db =
  ( PgTable table db,
    B.FromBackendRow Postgres (table Identity)
  )

type RunReadablePgTable table db =
  ( ReadablePgTable table db,
    ET.JSONEx (table Identity),
    DB.Config ET.PostgresConfig
  )

run ::
  ( ET.JSONEx a,
    DB.Config ET.PostgresConfig
  ) =>
  EL.SqlDB Pg a ->
  Flow (ET.DBResult a)
run query = do
  sqlConfig <- DB.getPgDBConfig DB.theConfig
  connection <- DB.getOrInitConn sqlConfig
  EL.runDB connection query

findOne ::
  RunReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  Flow (ET.DBResult (Maybe (table Identity)))
findOne dbTable predicate = run $ findOne' dbTable predicate

findOne' ::
  ReadablePgTable table db =>
  B.DatabaseEntity Postgres db (B.TableEntity table) ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  EL.SqlDB Pg (Maybe (table Identity))
findOne' dbTable predicate =
  EL.findRow $ B.select $ B.filter_ predicate $ B.all_ dbTable

throwDBError :: ET.DBError -> Flow a
throwDBError err =
  EL.logError @Text "DB error: " (show err)
    >> EL.throwException err500

findAllWithLimitOffsetWhere dbTable predicate limit offset orderBy =
  run $ EL.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ B.filter_ predicate $ B.all_ dbTable
