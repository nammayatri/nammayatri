{-# LANGUAGE AllowAmbiguousTypes #-}

module Tools.Auth (module Tools.Auth, module Verify, module Server) where

import Beckn.Prelude
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import Servant hiding (throwError)
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth.Server as Server
import Tools.Auth.Verify as Verify
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ServerAuth r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
type TokenAuth al = HeaderAuthWithPayload "token" VerifyToken al

-- | Performs token verification with checking server access.
type ServerAuth sn = HeaderAuthWithPayload "token" VerifyServer sn

data VerifyToken

data VerifyServer

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id DP.Person
  verificationDescription =
    "Checks whether token is registered and checks person api access.\
    \If you don't have a token, use registration endpoints."

instance VerificationMethod VerifyServer where
  type VerificationResult VerifyServer = DR.ServerName
  verificationDescription =
    "Checks whether token is registered and checks person server access.\
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyToken where
  type VerificationPayloadType VerifyToken = DMatrix.RequiredAccessLevel

instance VerificationMethodWithPayload VerifyServer where
  type VerificationPayloadType VerifyServer = DR.ServerName

verifyTokenAction ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  VerificationActionWithPayload VerifyToken m
verifyTokenAction = VerificationActionWithPayload verifyPerson

verifyServerAction ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  VerificationActionWithPayload VerifyServer m
verifyServerAction = VerificationActionWithPayload verifyServer

-- TODO We can use Handle pattern or class to avoid code duplication
verifyPerson ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  DMatrix.RequiredAccessLevel ->
  RegToken ->
  m (Id DP.Person)
verifyPerson requiredAccessLevel token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbTuple <- getKeyRedis key
  personId <- case mbTuple of
    Just (personId, _serverName) -> return personId
    Nothing -> do
      sr <- verifyToken token
      let personId = sr.personId
      let serverName = sr.serverName
      setExRedis key (personId, serverName) authTokenCacheExpiry
      return personId
  Verify.verifyAccessLevel requiredAccessLevel personId

verifyServer ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  DR.ServerName ->
  RegToken ->
  m DR.ServerName
verifyServer requiredServerAccess token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbTuple <- getKeyRedis key
  serverName <- case mbTuple of
    Just (_personId, serverName) -> return serverName
    Nothing -> do
      sr <- verifyToken token
      let personId = sr.personId
      let serverName = sr.serverName
      setExRedis key (personId, serverName) authTokenCacheExpiry
      return serverName
  unless (requiredServerAccess == serverName) $ throwError AccessDenied
  return serverName

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
