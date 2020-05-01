module Beckn.Product.Comment where

import           Beckn.Storage.Queries.Comment         as Comment
import           Beckn.Types.API.Comment
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Comment           as Comment
import           Beckn.Types.Storage.RegistrationToken
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           EulerHS.Prelude

import qualified EulerHS.Language                      as L

create :: Maybe RegistrationTokenText -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq{..} = withFlowHandler $ do
  RegistrationToken{..} <- verifyToken regToken
  id <- generateGUID
  comment <- getComment id  _entityType _EntityId
  Comment.create comment
  Comment.findById id
    >>= fromMaybeM500 "Unable to create Comment"
    >>= return . CreateRes
  where
    getComment id entityType entityId = do
      now <- getCurrTime
      return $ Comment.Comment
          { _id = id
          , _CommentedBy = entityId
          , _commentedByEntityType = entityType
          , _createdAt = now
          , _updatedAt = now
          , _value = _comment
          , ..
          }

list :: Maybe RegistrationTokenText -> Text -> Text -> FlowHandler ListRes
list regToken commentedOnEntityType commentedOnEntityId = withFlowHandler $ do
  RegistrationToken{..} <- verifyToken regToken
  Comment.findAllByCommentedOnEntity commentedOnEntityType commentedOnEntityId
    >>= return . ListRes
