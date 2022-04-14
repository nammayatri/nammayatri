{-# LANGUAGE AllowAmbiguousTypes #-}

module Beckn.Storage.Esqueleto.Queries
  ( module Beckn.Storage.Esqueleto.Queries,
    module EsqExport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Class
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Esqueleto.SqlDB
import Beckn.Storage.Esqueleto.Transactionable
import Database.Esqueleto.Experimental as EsqExport hiding
  ( delete,
    deleteCount,
    deleteKey,
    insert,
    insertSelect,
    insertSelectCount,
    repsert,
    select,
    selectOne,
    update,
    updateCount,
    upsert,
    upsertBy,
    (<&>),
  )
import qualified Database.Esqueleto.Experimental as Esq
import qualified Database.Esqueleto.Internal.Internal as Esq
import Database.Persist.Postgresql

findOne :: (Transactionable m, Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> m (Maybe a)
findOne q = runTransaction $ traverse toResult =<< lift selectOnlyOne
  where
    selectOnlyOne = do
      list <- Esq.select q
      case list of
        [res] -> return $ Just res
        _ -> return Nothing

-- FIXME more than one result should cause an exception, it's not Nothing

findById :: forall t a m. (Transactionable m, TEntity t a, TEntityKey t) => DomainKey t -> m (Maybe a)
findById dkey = runTransaction . findOne $ do
  let key = toKey dkey
  res <- from $ table @t
  where_ $ res Esq.^. persistIdField Esq.==. val key
  return res

findAll :: (Transactionable m, Esq.SqlSelect b t, QEntity t a) => Esq.SqlQuery b -> m [a]
findAll q = runTransaction $ traverse toResult =<< lift (Esq.select q)

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

createMany ::
  ( EsqDBFlow m r,
    TEntity t a
  ) =>
  [a] ->
  m ()
createMany = runTransaction . createMany'

createMany' ::
  ( TEntity t a
  ) =>
  [a] ->
  SqlDB ()
createMany' q = lift $ Esq.insertMany_ (toTType <$> q)

createIgnoreConflicts :: (EsqDBFlow m r, TEntity t a, AtLeastOneUniqueKey t) => a -> m ()
createIgnoreConflicts = runTransaction . createIgnoreConflicts'

createIgnoreConflicts' :: (TEntity t a, AtLeastOneUniqueKey t) => a -> SqlDB ()
createIgnoreConflicts' q = void $ lift $ Esq.insertBy (toTType q)

update ::
  ( Transactionable m,
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

repsert ::
  ( EsqDBFlow m r,
    TEntity t a,
    TEntityKey t
  ) =>
  DomainKey t ->
  a ->
  m ()
repsert r = runTransaction . repsert' r

repsert' ::
  ( TEntity t a,
    TEntityKey t
  ) =>
  DomainKey t ->
  a ->
  SqlDB ()
repsert' k v = lift (Esq.repsert (toKey k) (toTType v))

upsert ::
  ( EsqDBFlow m r,
    OnlyOneUniqueKey t,
    TEntity t a
  ) =>
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  m ()
upsert r = runTransaction . upsert' r

upsert' ::
  ( OnlyOneUniqueKey t,
    TEntity t a
  ) =>
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  SqlDB ()
upsert' r u = do
  let uniqueKey = onlyUniqueP $ toTType r
  upsertBy' uniqueKey r u

upsertBy ::
  ( EsqDBFlow m r,
    TEntity t a
  ) =>
  Unique t ->
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  m ()
upsertBy k r = runTransaction . upsertBy' k r

upsertBy' ::
  ( TEntity t a
  ) =>
  Unique t ->
  a ->
  [SqlExpr (Entity t) -> SqlExpr Esq.Update] ->
  SqlDB ()
upsertBy' k r u = do
  mbEntity <- lift $ getBy k
  case mbEntity of
    Nothing -> create' r
    Just ent -> update' $ \tbl -> do
      Esq.set
        tbl
        u
      where_ $ tbl Esq.^. persistIdField Esq.==. val (entityKey ent)

insertSelect ::
  ( EsqDBFlow m r,
    TEntity t a,
    PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  m ()
insertSelect = runTransaction . insertSelect'

insertSelect' ::
  ( TEntity t a,
    PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  SqlDB ()
insertSelect' = lift . Esq.insertSelect

insertSelectCount ::
  ( EsqDBFlow m r,
    TEntity t a,
    PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  m Int64
insertSelectCount = runTransaction . insertSelectCount'

insertSelectCount' ::
  ( TEntity t a,
    PersistEntity t
  ) =>
  SqlQuery (SqlExpr (Esq.Insertion t)) ->
  SqlDB Int64
insertSelectCount' = lift . Esq.insertSelectCount

(<#>) :: SqlExpr (Esq.Insertion (a -> b)) -> SqlExpr (Value a) -> SqlExpr (Esq.Insertion b)
(<#>) = (Esq.<&>)

whenJust_ :: Maybe a -> (a -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
whenJust_ mbVal func = maybe (Esq.val True) func mbVal

whenTrue_ :: Bool -> SqlExpr (Value Bool) -> SqlExpr (Value Bool)
whenTrue_ bl func = bool (Esq.val True) func bl

updateWhenJust_ :: (a -> SqlExpr (Entity e) -> SqlExpr Esq.Update) -> Maybe a -> [SqlExpr (Entity e) -> SqlExpr Esq.Update]
updateWhenJust_ f = maybe [] (\value -> [f value])

maybe_ ::
  forall a b.
  (PersistField a, PersistField b) =>
  SqlExpr (Value b) ->
  (SqlExpr (Value a) -> SqlExpr (Value b)) ->
  SqlExpr (Value (Maybe a)) ->
  SqlExpr (Value b)
maybe_ def f mbVal =
  case_
    [when_ (isNothing mbVal) then_ def]
    ( else_ $ f $ Esq.veryUnsafeCoerceSqlExprValue mbVal
    )
