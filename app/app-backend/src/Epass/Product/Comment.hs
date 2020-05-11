module Epass.Product.Comment where

import Epass.Storage.Queries.Comment as Comment
import Epass.Types.API.Comment
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Comment as Comment
import Beckn.Types.Storage.RegistrationToken
import Epass.Utils.Common
import Epass.Utils.Routes
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude

create :: Maybe RegistrationTokenText -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  RegistrationToken {..} <- verifyToken regToken
  id <- generateGUID
  comment <- getComment id _entityType _EntityId
  Comment.create comment
  Comment.findById id
    >>= fromMaybeM500 "Unable to create Comment"
    >>= return . CreateRes
  where
    getComment id entityType entityId = do
      now <- getCurrTime
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

list :: Maybe RegistrationTokenText -> Text -> Text -> FlowHandler ListRes
list regToken commentedOnEntityType commentedOnEntityId = withFlowHandler $ do
  RegistrationToken {..} <- verifyToken regToken
  Comment.findAllByCommentedOnEntity commentedOnEntityType commentedOnEntityId
    >>= return . ListRes
