module API.Registration where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import Environment
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR

data LoginReq = LoginReq
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data LoginRes = LoginRes
  { authToken :: Text,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

login :: LoginReq -> FlowHandler LoginRes
login LoginReq {..} = withFlowHandlerAPI $ do
  person <- QP.findByEmailAndPassword email password >>= fromMaybeM (PersonNotFound email)
  token <- generateToken person.id
  pure $ LoginRes token "Logged in successfully"

generateToken :: EsqDBFlow m r => Id DP.Person -> m Text
generateToken personId = do
  regToken <- buildRegistrationToken personId
  -- Clean old login session
  DB.runTransaction $ do
    QR.deleteByPersonId personId
    QR.create regToken
  pure $ regToken.token

logout :: Id DP.Person -> FlowHandler LogoutRes
logout personId = withFlowHandlerAPI $ do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  DB.runTransaction (QR.deleteByPersonId person.id)
  pure $ LogoutRes "Logged out successfully"

buildRegistrationToken :: MonadFlow m => Id DP.Person -> m DR.RegistrationToken
buildRegistrationToken personId = do
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  return $
    DR.RegistrationToken
      { id = Id rtid,
        token = token,
        personId = personId,
        createdAt = now
      }
