module Tools.Auth where

import Beckn.Prelude
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common as CoreCommon
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import Servant hiding (throwError)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (AdminTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

-- | Verifies admin's token.
type AdminTokenAuth = HeaderAuth "token" AdminVerifyToken

data VerifyToken

data AdminVerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id DP.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

instance VerificationMethod AdminVerifyToken where
  type VerificationResult AdminVerifyToken = DP.Person
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin role."

verifyTokenAction ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days]
  ) =>
  VerificationAction VerifyToken m
verifyTokenAction = VerificationAction verifyPerson

validateAdminAction ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days]
  ) =>
  VerificationAction AdminVerifyToken m
validateAdminAction = VerificationAction validateAdmin

verifyPerson ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days]
  ) =>
  RegToken ->
  m (Id DP.Person)
verifyPerson token = do
  let key = authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbPersonId <- Redis.getKeyRedis key
  case mbPersonId of
    Just personId -> return personId
    Nothing -> do
      sr <- verifyToken token
      let personId = sr.personId
      Redis.setExRedis key personId authTokenCacheExpiry
      return personId

-- FIXME it should be different for bap and bpp
authTokenCacheKey :: RegToken -> Text
authTokenCacheKey regToken =
  "BAP-dashboard:authTokenCacheKey:" <> regToken

validateAdmin ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["registrationTokenExpiry" ::: Days]
  ) =>
  RegToken ->
  m DP.Person
validateAdmin regToken = do
  DR.RegistrationToken {..} <- verifyToken regToken
  user <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  verifyAdmin user

verifyAdmin :: MonadFlow m => DP.Person -> m DP.Person
verifyAdmin user = do
  when (user.role /= DP.ADMIN) $
    throwError AccessDenied
  return user

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
