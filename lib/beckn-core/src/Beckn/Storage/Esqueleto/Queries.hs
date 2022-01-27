{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Storage.Esqueleto.Queries
  ( module Beckn.Storage.Esqueleto.Queries,
    module EsqExport,
  )
where

import Beckn.Storage.Esqueleto.Class
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Esqueleto.Logger (runLoggerIO)
import Beckn.Storage.Esqueleto.SqlDB
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
import EulerHS.Prelude hiding (Key, id)

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

findOne :: (EsqDBFlow m r, Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> m (Maybe a)
findOne q = runTransaction $ findOne' q

findOne' :: (Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> SqlDB (Maybe a)
findOne' q = traverse toResult =<< lift selectOnlyOne
  where
    selectOnlyOne = do
      list <- Esq.select q
      case list of
        [res] -> return $ Just res
        _ -> return Nothing

findById :: (EsqDBFlow m r, TEntity t a, TEntityKey t) => DomainKey t -> m (Maybe a)
findById id = runTransaction $ findById' id

findById' ::
  forall t a.
  ( TEntity t a,
    TEntityKey t
  ) =>
  DomainKey t ->
  SqlDB (Maybe a)
findById' dkey = findOne' $ do
  let key = toKey dkey
  res <- from $ table @t
  where_ $ res Esq.^. persistIdField Esq.==. val key
  return res

findAll :: (EsqDBFlow m r, Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> m [a]
findAll q = runTransaction $ findAll' q

findAll' :: (Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> SqlDB [a]
findAll' q = traverse toResult =<< lift (Esq.select q)

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

createIgnoreConflicts' :: (TEntity t a, AtLeastOneUniqueKey t) => a -> SqlDB ()
createIgnoreConflicts' q = void $ lift $ Esq.insertBy (toTType q)

createIgnoreConflicts :: (EsqDBFlow m r, TEntity t a, AtLeastOneUniqueKey t) => a -> m ()
createIgnoreConflicts = runTransaction . createIgnoreConflicts'

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

deleteByKey ::
  forall t r (m :: Type -> Type).
  ( EsqDBFlow m r,
    TEntityKey t
  ) =>
  DomainKey t ->
  m ()
deleteByKey = runTransaction . deleteByKey' @t

deleteByKey' ::
  forall t.
  TEntityKey t =>
  DomainKey t ->
  SqlDB ()
deleteByKey' = lift . Esq.deleteKey . toKey @t

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

whenJust_ :: Maybe a -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
whenJust_ mbVal func = maybe (Esq.val True) func mbVal
