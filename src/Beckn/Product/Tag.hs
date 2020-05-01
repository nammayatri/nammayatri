module Beckn.Product.Tag where

import           Beckn.Storage.Queries.EntityTag       as EntityTag
import           Beckn.Storage.Queries.Tag             as Tag
import           Beckn.Types.API.Tag
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.EntityTag         as EntityTag
import           Beckn.Types.Storage.RegistrationToken
import qualified Beckn.Types.Storage.Tag               as Tag
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           EulerHS.Prelude

import qualified EulerHS.Language                      as L

create :: Maybe RegistrationTokenText -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq{..} = withFlowHandler $ do
  RegistrationToken{..} <- verifyToken regToken
  id <- generateGUID
  let
    createByEntityType = _entityType
    createdById = _EntityId
  tag <- getTag id createdById createByEntityType
  Tag.create tag
  Tag.findById id
    >>= fromMaybeM500 "Couldn't create Tag"
    >>= return . CreateRes
  where
    getTag id createdById createByEntityType = do
      now <- getCurrTime
      return $ Tag.Tag
        { _id = id
        , _CreatedBy = createdById
        , _createdByEntityType = createByEntityType
        , _createdAt = now
        , _updatedAt = now
        , ..
        }

list :: Maybe RegistrationTokenText -> Text -> Text -> FlowHandler ListRes
list regToken entityType entityId = withFlowHandler $ do
  verifyToken regToken
  undefined

tagEntity ::  Maybe RegistrationTokenText -> TagEntityReq -> FlowHandler TagEntityRes
tagEntity regToken TagEntityReq{..} = withFlowHandler $ do
  L.logInfo "tagEntity" "In here"
  RegistrationToken{..} <- verifyToken regToken

  Tag.findById (TagId _TagId)
    >>= fromMaybeM400 "Invalid Tag id"
  id <- generateGUID
  let
    createByEntityType = _entityType
    createdById = _EntityId

  --TODO add checks for entity
  entityTag <- getEntityTag id createdById createByEntityType
  EntityTag.create entityTag
  EntityTag.findById id
    >>= fromMaybeM500 "Couldn't create EntityTag"
    >>= return . TagEntityRes
  where
    getEntityTag id createdById createByEntityType = do
      now <- getCurrTime
      return $ EntityTag.EntityTag
        { _id = id
        , _TaggedBy = createByEntityType
        , _taggedByEntityId = createdById
        , _createdAt = now
        , _updatedAt = now
        , _info = Nothing
        , ..
        }
