module Tools.Auth.Common (verifyPerson, cleanCachedTokens) where

import Beckn.Prelude
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Beckn.Utils.Common as Utils
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import qualified Storage.Queries.RegistrationToken as QR

verifyPerson ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  RegToken ->
  m (Id DP.Person, DR.ServerName)
verifyPerson token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbTuple <- getKeyRedis key
  (personId, serverName) <- case mbTuple of
    Just (personId, serverName) -> return (personId, serverName)
    Nothing -> do
      sr <- verifyToken token
      let personId = sr.personId
      let serverName = sr.serverName
      setExRedis key (personId, serverName) authTokenCacheExpiry
      return (personId, serverName)
  return (personId, serverName)

getKeyRedis :: (MonadFlow m, MonadThrow m, Log m) => Text -> m (Maybe (Id DP.Person, DR.ServerName))
getKeyRedis = Redis.getKeyRedis

setExRedis :: (MonadFlow m, MonadThrow m, Log m) => Text -> (Id DP.Person, DR.ServerName) -> Int -> m ()
setExRedis = Redis.setExRedis

authTokenCacheKey ::
  HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text] =>
  RegToken ->
  m Text
authTokenCacheKey regToken = do
  authTokenCacheKeyPrefix <- asks (.authTokenCacheKeyPrefix)
  pure $ authTokenCacheKeyPrefix <> regToken

verifyToken ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  RegToken ->
  m DR.RegistrationToken
verifyToken regToken = do
  QR.findByToken regToken
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateToken ::
  HasFlowEnv m r '["registrationTokenExpiry" ::: Days] =>
  DR.RegistrationToken ->
  m DR.RegistrationToken
validateToken sr@DR.RegistrationToken {..} = do
  registrationTokenExpiry <- asks (.registrationTokenExpiry)
  let nominal = realToFrac . daysToSeconds $ registrationTokenExpiry
  expired <- Utils.isExpired nominal createdAt
  when expired $ Utils.throwError TokenExpired
  return sr

-- TODO two endpoints for logout: 1. from one server 2. from all servers
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
