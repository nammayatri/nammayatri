module Epass.Product.Comment where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Storage.RegistrationToken
import Beckn.Utils.Common
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Epass.Storage.Queries.Comment as Comment
import Epass.Types.API.Comment
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Comment as Comment
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude

create :: RegToken -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  RegistrationToken {..} <- verifyToken regToken
  id <- generateGUID
  comment <- getComment id _entityType _EntityId
  Comment.create comment
  CreateRes
    <$> ( Comment.findById id
            >>= fromMaybeM500 "Unable to create Comment"
        )
  where
    getComment id entityType entityId = do
      now <- getCurrentTimeUTC
      return $
        Comment.Comment
          { _id = id,
            _CommentedBy = entityId,
            _commentedByEntityType = entityType,
            _createdAt = now,
            _updatedAt = now,
            _value = _comment,
            ..
          }

list :: RegToken -> Text -> Text -> FlowHandler ListRes
list regToken commentedOnEntityType commentedOnEntityId = withFlowHandler $ do
  RegistrationToken {..} <- verifyToken regToken
  ListRes
    <$> Comment.findAllByCommentedOnEntity commentedOnEntityType commentedOnEntityId
