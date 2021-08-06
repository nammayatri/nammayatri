module Utils.Auth where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra (HasField)
import Servant hiding (Context)
import Storage.Queries.Organization (findOrgByShortId)
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.Error
import qualified Types.Storage.Organization as SOrganization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.RegistrationToken as SR

type VerificationAPIKey = APIKeyAuth VerifyAPIKey

data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = SOrganization.Organization
  verificationDescription =
    "Checks whether gateway/provider is registered.\
    \If you don't have an API key, register the gateway/provider."

type LookupRegistryOrg = (HttpSig.LookupRegistry SOrganization.Organization)

verifyApiKey :: DBFlow m r => VerificationAction VerifyAPIKey m
verifyApiKey = VerificationAction QOrganization.verifyApiKey

lookup :: (DBFlow m r, HttpSig.AuthenticatingEntity r) => HttpSig.LookupAction LookupRegistryOrg m
lookup = HttpSig.lookupRegistryAction findOrgByShortId

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPerson :: (DBFlow m r, HasField "authTokenCacheExpiry" r Second) => RegToken -> m (Id Person.Person)
verifyPerson token = do
  let key = authTokenCacheKey token
  authTokenCacheExpiry <- getSecond <$> asks (.authTokenCacheExpiry)
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
  "BAP:authTokenCacheKey:" <> regToken

verifyPersonAction :: (DBFlow m r, HasField "authTokenCacheExpiry" r Second) => VerificationAction VerifyToken m
verifyPersonAction = VerificationAction verifyPerson

verifyToken :: DBFlow m r => RegToken -> m SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token
    >>= Utils.fromMaybeM (InvalidToken token)
    >>= validateToken

validateToken :: DBFlow m r => SR.RegistrationToken -> m SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal updatedAt
  when expired $ Utils.throwError TokenExpired
  return sr
