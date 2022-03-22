module Utils.Auth where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Utils.Common as CoreCommon
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import Data.Text as T
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import Servant hiding (throwError)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.Error

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

verifyTokenAction :: (EsqDBFlow m r, HasField "authTokenCacheExpiry" r Seconds) => VerificationAction VerifyToken m
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

verifyToken :: EsqDBFlow m r => RegToken -> m SR.RegistrationToken
verifyToken regToken = do
  QR.findByToken regToken
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateAdmin :: (EsqDBFlow m r, EncFlow m r) => RegToken -> m Person.Person
validateAdmin regToken = do
  SR.RegistrationToken {..} <- verifyToken regToken
  user <-
    QP.findById (Id entityId)
      >>= fromMaybeM PersonNotFound
  verifyAdmin user

verifyPerson :: (EsqDBFlow m r, HasField "authTokenCacheExpiry" r Seconds) => RegToken -> m (Id Person.Person)
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

validateAdminAction :: (EsqDBFlow m r, EncFlow m r) => VerificationAction AdminVerifyToken m
validateAdminAction = VerificationAction validateAdmin

validateToken :: EsqDBFlow m r => SR.RegistrationToken -> m SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal updatedAt
  unless verified $ throwError TokenIsNotVerified
  when expired $ Utils.throwError TokenExpired
  return sr
