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
import Beckn.Types.Logging (HasLog)
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
  (EsqDBFlow m r, HasLog r) =>
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

findOne :: (EsqDBFlow m r, HasLog r, Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> m (Maybe a)
findOne q = runTransaction $ findOne' q

findOne' :: (Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> SqlDB (Maybe a)
findOne' q = traverse fromTEntity =<< lift (Esq.selectOne q)

findAll :: (EsqDBFlow m r, HasLog r, Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> m [a]
findAll q = runTransaction $ findAll' q

findAll' :: (Esq.SqlSelect b (Esq.Entity t), TEntity t a) => Esq.SqlQuery b -> SqlDB [a]
findAll' q = traverse fromTEntity =<< lift (Esq.select q)

create ::
  ( EsqDBFlow m r,
    HasLog r,
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

createReturningKey ::
  ( EsqDBFlow m r,
    HasLog r,
    TEntity t a,
    TEntityKey t a
  ) =>
  a ->
  m (Id a)
createReturningKey = runTransaction . createReturningKey'

createReturningKey' ::
  (TEntityKey t b, TEntity t a) =>
  a ->
  SqlDB (Id b)
createReturningKey' q = fromKey <$> lift (Esq.insert $ toTType q)

createReturningEntity ::
  ( EsqDBFlow m r,
    HasLog r,
    TEntity t a,
    TEntityKey t a
  ) =>
  a ->
  m a
createReturningEntity = runTransaction . createReturningEntity'

createReturningEntity' ::
  (TEntityKey t a, TEntity t a) =>
  a ->
  SqlDB a
createReturningEntity' q = do
  let tType = toTType q
  key <- lift $ Esq.insert tType
  fromTEntity $ Entity key tType

update ::
  ( EsqDBFlow m r,
    HasLog r,
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
    HasLog r,
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

deleteByKey ::
  ( EsqDBFlow m r,
    HasLog r,
    TEntityKey t a
  ) =>
  a ->
  m ()
deleteByKey = runTransaction . deleteByKey'

deleteByKey' ::
  TEntityKey t a =>
  a ->
  SqlDB ()
deleteByKey' = lift . Esq.deleteKey . toKey

delete ::
  (EsqDBFlow m r, HasLog r) =>
  Esq.SqlQuery () ->
  m ()
delete = runTransaction . delete'

delete' ::
  Esq.SqlQuery () ->
  SqlDB ()
delete' = lift . Esq.delete

deleteReturningCount ::
  (EsqDBFlow m r, HasLog r) =>
  Esq.SqlQuery () ->
  m Int64
deleteReturningCount = runTransaction . deleteReturningCount'

deleteReturningCount' ::
  Esq.SqlQuery () ->
  SqlDB Int64
deleteReturningCount' = lift . Esq.deleteCount

upsert ::
  ( EsqDBFlow m r,
    HasLog r,
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
    HasLog r,
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
