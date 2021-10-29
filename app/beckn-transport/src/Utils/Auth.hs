module Utils.Auth where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Common as CoreCommon
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Beckn.Utils.Servant.RegistryService as RegistryService
import Beckn.Utils.Servant.SignatureAuth
import Data.Text as T
import EulerHS.Prelude hiding (id)
import Servant hiding (throwError)
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.Error
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Person as SP
import qualified Types.Storage.RegistrationToken as SR

-- | TODO: Perform some API key verification.
data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = Organization
  verificationDescription =
    "Checks whether app/gateway is registered.\
    \If you don't have an API key, register the app/gateway."

type LookupRegistryOrg = LookupRegistry Organization

verifyApiKey :: DBFlow m r => VerificationAction VerifyAPIKey m
verifyApiKey = VerificationAction $ Org.findOrgByApiKey >=> fromMaybeM OrgNotFound

lookup ::
  ( DBFlow m r,
    HasFlowEnv m r '["registryUrl" ::: BaseUrl],
    AuthenticatingEntity r,
    CoreMetrics m
  ) =>
  LookupAction LookupRegistryOrg m
lookup = RegistryService.decodeViaRegistry Org.findOrgByShortId

getHttpManagerKey :: Text -> String
getHttpManagerKey keyId = signatureAuthManagerKey <> "-" <> T.unpack keyId

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

data VerifyToken = VerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyTokenAction :: (DBFlow m r, HasField "authTokenCacheExpiry" r Seconds) => VerificationAction VerifyToken m
verifyTokenAction = VerificationAction verifyPerson

-- | Verifies admin's token.
type AdminTokenAuth = HeaderAuth "token" AdminVerifyToken

data AdminVerifyToken

instance VerificationMethod AdminVerifyToken where
  type VerificationResult AdminVerifyToken = Person.Person
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin role."

verifyAdmin :: MonadFlow m => SP.Person -> m Person.Person
verifyAdmin user = do
  when (user.role /= SP.ADMIN) $
    throwError AccessDenied
  case user.organizationId of
    Just _ -> return user
    Nothing -> throwError (PersonFieldNotPresent "organization_id")

verifyToken :: DBFlow m r => RegToken -> m SR.RegistrationToken
verifyToken regToken = do
  QR.findRegistrationTokenByToken regToken
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateAdmin :: (DBFlow m r, EncFlow m r) => RegToken -> m Person.Person
validateAdmin regToken = do
  SR.RegistrationToken {..} <- verifyToken regToken
  user <-
    QP.findPersonById (Id entityId)
      >>= fromMaybeM PersonNotFound
  verifyAdmin user

verifyPerson :: (DBFlow m r, HasField "authTokenCacheExpiry" r Seconds) => RegToken -> m (Id Person.Person)
verifyPerson token = do
  let key = authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbPersonId <- Redis.getKeyRedis key
  case mbPersonId of
    Just personId -> return personId
    Nothing -> do
      sr <- verifyToken token
      let expiryTime = min sr.tokenExpiry authTokenCacheExpiry
      let personId = Id sr.entityId
      Redis.setExRedis key personId expiryTime
      return personId

authTokenCacheKey :: RegToken -> Text
authTokenCacheKey regToken =
  "BPP:authTokenCacheKey:" <> regToken

validateAdminAction :: (DBFlow m r, EncFlow m r) => VerificationAction AdminVerifyToken m
validateAdminAction = VerificationAction validateAdmin

validateToken :: DBFlow m r => SR.RegistrationToken -> m SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal updatedAt
  unless verified $ throwError TokenIsNotVerified
  when expired $ Utils.throwError TokenExpired
  return sr
