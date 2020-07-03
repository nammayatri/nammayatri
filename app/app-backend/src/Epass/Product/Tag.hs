module Epass.Product.Tag where

import Beckn.Types.Common
import Beckn.Types.Storage.RegistrationToken
import Beckn.Utils.Common
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Epass.Storage.Queries.EntityTag as EntityTag
import Epass.Storage.Queries.Tag as Tag
import Epass.Types.API.Tag
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.EntityTag as EntityTag
import qualified Epass.Types.Storage.Tag as Tag
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude

create :: RegToken -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  RegistrationToken {..} <- verifyToken regToken
  id <- generateGUID
  let createByEntityType = _entityType
      createdById = _EntityId
  tag <- getTag id createdById createByEntityType
  Tag.create tag
  CreateRes
    <$> ( Tag.findById id
          >>= fromMaybeM500 "Couldn't create Tag"
        )
  where
    getTag id createdById createByEntityType = do
      now <- getCurrentTimeUTC
      return $
        Tag.Tag
          { _id = id,
            _CreatedBy = createdById,
            _createdByEntityType = createByEntityType,
            _createdAt = now,
            _updatedAt = now,
            ..
          }

list ::
  RegToken ->
  [TagId] ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler ListRes
list regToken tagIds entityTypeM entityIdM = withFlowHandler $ do
  verifyToken regToken
  res <-
    if not $ null tagIds
      then Tag.findAllById tagIds
      else
        if isJust entityTypeM && isJust entityIdM
          then Tag.findAllByEntity (fromJust entityTypeM) (fromJust entityIdM)
          else return []
  return $ ListRes res

tagEntity :: RegToken -> TagEntityReq -> FlowHandler TagEntityRes
tagEntity regToken TagEntityReq {..} = withFlowHandler $ do
  L.logInfo "tagEntity" "In here"
  RegistrationToken {..} <- verifyToken regToken
  Tag.findById (TagId _TagId)
    >>= fromMaybeM400 "Invalid Tag id"
  id <- generateGUID
  let createByEntityType = _entityType
      createdById = _EntityId
  --TODO add checks for entity
  entityTag <- getEntityTag id createdById createByEntityType
  EntityTag.create entityTag
  TagEntityRes
    <$> ( EntityTag.findById id
          >>= fromMaybeM500 "Couldn't create EntityTag"
        )
  where
    getEntityTag id createdById createByEntityType = do
      now <- getCurrentTimeUTC
      return $
        EntityTag.EntityTag
          { _id = id,
            _TaggedBy = createByEntityType,
            _taggedByEntityId = createdById,
            _createdAt = now,
            _updatedAt = now,
            _info = Nothing,
            ..
          }

listTypes ::
  RegToken ->
  FlowHandler ListVal
listTypes regToken = withFlowHandler $ do
  verifyToken regToken
  ListVal <$> Tag.findAllTagTypes

listTags ::
  RegToken ->
  Text ->
  FlowHandler ListVal
listTags regToken tagType = withFlowHandler $ do
  verifyToken regToken
  ListVal <$> Tag.findAllTagWhereType tagType

listByTag ::
  RegToken ->
  Text ->
  Text ->
  FlowHandler ListRes
listByTag regToken tagType tag = withFlowHandler $ do
  verifyToken regToken
  ListRes <$> Tag.findAllByTag tagType tag
