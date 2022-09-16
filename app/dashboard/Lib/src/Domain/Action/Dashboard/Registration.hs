module Domain.Action.Dashboard.Registration where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth (authTokenCacheKey)

data LoginReq = LoginReq
  { email :: Text,
    password :: Text,
    bppName :: DR.ServerName
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data LoginRes = LoginRes
  { authToken :: Text,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

login ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    EncFlow m r
  ) =>
  LoginReq ->
  m LoginRes
login LoginReq {..} = do
  person <- QP.findByEmailAndPassword email password >>= fromMaybeM (PersonDoesNotExist email)
  token <- generateToken person.id bppName
  pure $ LoginRes token "Logged in successfully"

generateToken ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  DR.ServerName ->
  m Text
generateToken personId bppName = do
  regToken <- buildRegistrationToken personId bppName
  -- Clean old login session
  cleanCachedTokens personId
  DB.runTransaction $ do
    QR.deleteAllByPersonId personId
    QR.create regToken
  pure $ regToken.token

logout ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  m LogoutRes
logout personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  cleanCachedTokens personId
  DB.runTransaction (QR.deleteAllByPersonId person.id)
  pure $ LogoutRes "Logged out successfully"

buildRegistrationToken :: MonadFlow m => Id DP.Person -> DR.ServerName -> m DR.RegistrationToken
buildRegistrationToken personId serverName = do
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  return $
    DR.RegistrationToken
      { id = Id rtid,
        token = token,
        personId = personId,
        serverName = serverName,
        createdAt = now
      }

cleanCachedTokens ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  m ()
cleanCachedTokens personId = do
  regTokens <- QR.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    key <- authTokenCacheKey regToken.token
    void $ Redis.deleteKeyRedis key
