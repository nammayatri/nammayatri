{-# LANGUAGE AllowAmbiguousTypes #-}

module Tools.Auth where

import Beckn.Prelude
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import Servant hiding (throwError)
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Roles as Roles
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking person role.
type TokenAuth pr = HeaderAuthWithPayload "token" VerifyToken pr

data VerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id DP.Person
  verificationDescription =
    "Checks whether token is registered and checks person access.\
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyToken where
  type VerificationPayloadType VerifyToken = Roles.RequiredAccessLevel

verifyTokenAction ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  VerificationActionWithPayload VerifyToken m
verifyTokenAction = VerificationActionWithPayload verifyPerson

verifyPerson ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Roles.RequiredAccessLevel ->
  RegToken ->
  m (Id DP.Person)
verifyPerson requiredAccessLevel token = do
  key <- authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbPersonId <- Redis.getKeyRedis key
  personId <- case mbPersonId of
    Just personId -> return personId
    Nothing -> do
      sr <- verifyToken token
      let personId = sr.personId
      Redis.setExRedis key personId authTokenCacheExpiry
      return personId
  Roles.verifyAccessLevel requiredAccessLevel personId

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
