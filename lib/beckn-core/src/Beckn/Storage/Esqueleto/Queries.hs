module Beckn.Storage.Esqueleto.Queries
  ( module Beckn.Storage.Esqueleto.Queries,
    module EsqExport,
  )
where

import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Esqueleto.Logger (runLoggerIO)
import Beckn.Storage.Esqueleto.SqlDB
import Beckn.Storage.Esqueleto.Types
import Beckn.Types.Id (Id)
import Beckn.Types.Time (getCurrentTime)
import Database.Esqueleto.Experimental as EsqExport hiding
  ( delete,
    deleteCount,
    deleteKey,
    insert,
    select,
    selectOne,
    update,
    updateCount,
    upsert,
    upsertBy,
  )
import qualified Database.Esqueleto.Experimental as Esq
import qualified Database.Esqueleto.Internal.Internal as Esq
import Database.Persist.Postgresql
import EulerHS.Prelude hiding (Key)

runTransaction ::
  (EsqDBFlow m r) =>
  SqlDB a ->
  m a
runTransaction run = do
  logEnv <- asks (.loggerEnv)
  dbEnv <- asks (.esqDBEnv)
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now
          }
  liftIO . runLoggerIO logEnv $ runSqlPool (runReaderT run sqlDBEnv) dbEnv.connPool

findOne :: (EsqDBFlow m r, Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> m (Maybe a)
findOne q = runTransaction $ findOne' q

findOne' :: (Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> SqlDB (Maybe a)
findOne' q = traverse fromTEntity =<< lift selectOnlyOne
  where
    selectOnlyOne = do
      list <- Esq.select q
      case list of
        [res] -> return $ Just res
        _ -> return Nothing

findAll :: (EsqDBFlow m r, Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> m [a]
findAll q = runTransaction $ findAll' q

findAll' :: (Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> SqlDB [a]
findAll' q = traverse fromTEntity =<< lift (Esq.select q)

create ::
  ( EsqDBFlow m r,
    TEntity t a
  ) =>
  a ->
  m ()
create = runTransaction . create'

create' ::
  ( TEntity t a
  ) =>
  a ->
  SqlDB ()
create' q = lift $ Esq.insert_ (toTType q)

update ::
  ( EsqDBFlow m r,
    PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  m ()
update = runTransaction . update'

update' ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  SqlDB ()
update' = lift . Esq.update

updateReturningCount ::
  ( EsqDBFlow m r,
    PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  m Int64
updateReturningCount = runTransaction . updateReturningCount'

updateReturningCount' ::
  ( PersistEntity a,
    BackendCompatible SqlBackend (PersistEntityBackend a)
  ) =>
  (Esq.SqlExpr (Entity a) -> Esq.SqlQuery ()) ->
  SqlDB Int64
updateReturningCount' = lift . Esq.updateCount

deleteById ::
  ( EsqDBFlow m r,
    TEntityKey t a
  ) =>
  Id a ->
  m ()
deleteById = runTransaction . deleteById'

deleteById' ::
  TEntityKey t a =>
  Id a ->
  SqlDB ()
deleteById' = lift . Esq.deleteKey . toKey

delete ::
  (EsqDBFlow m r) =>
  Esq.SqlQuery () ->
  m ()
delete = runTransaction . delete'

delete' ::
  Esq.SqlQuery () ->
  SqlDB ()
delete' = lift . Esq.delete

deleteReturningCount ::
  (EsqDBFlow m r) =>
  Esq.SqlQuery () ->
  m Int64
deleteReturningCount = runTransaction . deleteReturningCount'

deleteReturningCount' ::
  Esq.SqlQuery () ->
  SqlDB Int64
deleteReturningCount' = lift . Esq.deleteCount

upsert ::
  ( EsqDBFlow m r,
    OnlyOneUniqueKey t,
    TEntity t a
  ) =>
  a ->
  [Update t] ->
  m a
upsert r = runTransaction . upsert' r

upsert' ::
  ( OnlyOneUniqueKey t,
    TEntity t a
  ) =>
  a ->
  [Update t] ->
  SqlDB a
upsert' r u = fromTEntity =<< lift (Esq.upsert (toTType r) u)

upsertBy ::
  ( EsqDBFlow m r,
    OnlyOneUniqueKey t,
    TEntity t a
  ) =>
  Unique t ->
  a ->
  [Update t] ->
  m a
upsertBy k r = runTransaction . upsertBy' k r

upsertBy' ::
  ( OnlyOneUniqueKey t,
    TEntity t a
  ) =>
  Unique t ->
  a ->
  [Update t] ->
  SqlDB a
upsertBy' k r u = fromTEntity =<< lift (Esq.upsertBy k (toTType r) u)
