module Domain.Action.Dashboard.Registration where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Validation
import Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.ServerAccess as QServer
import Tools.Auth
import qualified Tools.Auth.Common as Auth
import qualified Tools.Client as Client
import Tools.Validation

data LoginReq = LoginReq
  { email :: Text,
    password :: Text,
    serverName :: DR.ServerName
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data LoginRes = LoginRes
  { authToken :: Text,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

validateLoginReq :: [DR.ServerName] -> Validate LoginReq
validateLoginReq availableServerNames LoginReq {..} =
  sequenceA_
    [ validateField "serverName" serverName $ InList availableServerNames
    ]

login ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [Client.DataServer]],
    EncFlow m r
  ) =>
  LoginReq ->
  m LoginRes
login req@LoginReq {..} = do
  availableServers <- asks (.dataServers)
  runRequestValidation (validateLoginReq $ availableServers <&> (.name)) req
  person <- QP.findByEmailAndPassword email password >>= fromMaybeM (PersonDoesNotExist email)
  _serverAccess <- QServer.findByPersonIdAndServerName person.id serverName >>= fromMaybeM AccessDenied --FIXME cleanup tokens for this serverName
  token <- generateToken person.id serverName
  pure $ LoginRes token "Logged in successfully"

generateToken ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  DR.ServerName ->
  m Text
generateToken personId serverName = do
  regToken <- buildRegistrationToken personId serverName
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokensByServerName personId serverName
  DB.runTransaction $ do
    QR.deleteAllByPersonIdAndServerName personId serverName
    QR.create regToken
  pure $ regToken.token

logout ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logout tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokensByServerName personId tokenInfo.serverName
  DB.runTransaction (QR.deleteAllByPersonIdAndServerName person.id tokenInfo.serverName)
  pure $ LogoutRes "Logged out successfully"

logoutAllServers ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logoutAllServers tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokens personId
  DB.runTransaction (QR.deleteAllByPersonId person.id)
  pure $ LogoutRes "Logged out successfully from all servers"

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
