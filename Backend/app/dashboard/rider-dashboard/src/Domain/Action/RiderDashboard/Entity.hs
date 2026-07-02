module Domain.Action.RiderDashboard.Entity
  ( CreateEntityReq (..),
    CreateEntityResp (..),
    UpdateEntityReq (..),
    ListEntityResp (..),
    createEntity,
    listEntity,
    updateEntity,
    deleteEntity,
  )
where

import qualified "lib-dashboard" Domain.Types.Entity as DE
import qualified "lib-dashboard" Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" Storage.Beam.BeamFlow as BeamFlow
import qualified "lib-dashboard" Storage.Queries.Entity as QE
import "lib-dashboard" Tools.Error

data CreateEntityReq = CreateEntityReq
  { entityName :: Text,
    entityShortId :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype CreateEntityResp = CreateEntityResp
  { entityId :: Id DE.Entity
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data UpdateEntityReq = UpdateEntityReq
  { entityName :: Maybe Text,
    entityShortId :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype ListEntityResp = ListEntityResp {list :: [DE.Entity]}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

createEntity :: BeamFlow.BeamFlow m r => Id DP.Person -> CreateEntityReq -> m CreateEntityResp
createEntity actorPersonId req = do
  let shortIdTyped = ShortId req.entityShortId
  mbExisting <- QE.findByEntityShortId shortIdTyped
  whenJust mbExisting $ \_ ->
    throwError (InvalidRequest $ "Entity with entityShortId " <> req.entityShortId <> " already exists")
  entityId <- generateGUID
  now <- getCurrentTime
  let entity =
        DE.Entity
          { id = entityId,
            entityName = req.entityName,
            entityShortId = shortIdTyped,
            deleted = False,
            createdAt = now,
            updatedAt = now
          }
  QE.create entity
  logInfo $ "[Entity.create] actor=" <> actorPersonId.getId <> " entityId=" <> entityId.getId <> " shortId=" <> req.entityShortId
  pure $ CreateEntityResp entityId

listEntity :: BeamFlow.BeamFlow m r => Maybe Bool -> m ListEntityResp
listEntity mbIncludeDeleted =
  ListEntityResp <$> QE.findAllByFilters mbIncludeDeleted

updateEntity :: BeamFlow.BeamFlow m r => Id DP.Person -> Id DE.Entity -> UpdateEntityReq -> m APISuccess
updateEntity actorPersonId entityId req = do
  existing <- QE.findById entityId >>= fromMaybeM (InvalidRequest $ "Entity " <> entityId.getId <> " does not exist")
  when existing.deleted $
    throwError (InvalidRequest $ "Entity " <> entityId.getId <> " is soft-deleted; restore it before editing")
  whenJust req.entityShortId $ \newShortId -> do
    let newShortIdTyped = ShortId newShortId
    when (newShortIdTyped /= existing.entityShortId) $ do
      mbDup <- QE.findByEntityShortId newShortIdTyped
      whenJust mbDup $ \_ ->
        throwError (InvalidRequest $ "Entity with entityShortId " <> newShortId <> " already exists")
  now <- getCurrentTime
  let updated =
        existing
          { DE.entityName = fromMaybe existing.entityName req.entityName,
            DE.entityShortId = maybe existing.entityShortId ShortId req.entityShortId,
            DE.updatedAt = now
          }
  QE.updateByPrimaryKey updated
  logInfo $ "[Entity.update] actor=" <> actorPersonId.getId <> " entityId=" <> entityId.getId
  pure Success

-- Soft delete preserves person.entity_id references; `?includeDeleted=true`
-- on /entity/list is the only way to see these rows after the flip.
deleteEntity :: BeamFlow.BeamFlow m r => Id DP.Person -> Id DE.Entity -> m APISuccess
deleteEntity actorPersonId entityId = do
  existing <- QE.findById entityId >>= fromMaybeM (InvalidRequest $ "Entity " <> entityId.getId <> " does not exist")
  now <- getCurrentTime
  QE.updateByPrimaryKey existing {DE.deleted = True, DE.updatedAt = now}
  logInfo $ "[Entity.delete] actor=" <> actorPersonId.getId <> " entityId=" <> entityId.getId
  pure Success
