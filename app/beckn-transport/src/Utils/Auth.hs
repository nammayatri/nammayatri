module Utils.Auth where

import App.Types
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Types.Storage.Organization (Organization)
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common as CoreCommon
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.Servant.SignatureAuth
import Data.Text as T
import EulerHS.Prelude hiding (id)
import Servant hiding (throwError)
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.Error

-- | TODO: Perform some API key verification.
data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = Organization
  verificationDescription =
    "Checks whether app/gateway is registered.\
    \If you don't have an API key, register the app/gateway."

verifyApiKey :: VerificationAction VerifyAPIKey AppEnv
verifyApiKey = VerificationAction $ Org.findOrgByApiKey >=> fromMaybeM OrgNotFound

lookup :: LookupAction LookupRegistry AppEnv
lookup = lookupRegistryAction Org.findOrganizationByShortId

getHttpManagerKey :: Text -> String
getHttpManagerKey keyId = signatureAuthManagerKey <> "-" <> T.unpack keyId

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (DriverTokenAuth :> sub)
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
  type VerificationResult VerifyToken = SR.RegistrationToken
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyTokenAction :: VerificationAction VerifyToken AppEnv
verifyTokenAction = VerificationAction QR.verifyToken

-- | Verifies admin's token.
type AdminTokenAuth = HeaderAuth "token" AdminVerifyToken

data AdminVerifyToken

instance VerificationMethod AdminVerifyToken where
  type VerificationResult AdminVerifyToken = Text
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin role."

-- | Verifies admin or driver's token.
type DriverTokenAuth = HeaderAuth "token" DriverVerifyToken

data DriverVerifyToken

instance VerificationMethod DriverVerifyToken where
  type VerificationResult DriverVerifyToken = Text

  -- verifyToken = validateDriver
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin or driver role."

verifyAdmin :: MonadFlow m => SP.Person -> m Text
verifyAdmin user = do
  when (user.role /= SP.ADMIN) $
    throwError AccessDenied
  case user.organizationId of
    Just orgId -> return $ getId orgId
    Nothing -> throwError (PersonFieldNotPresent "organization_id")

verifyDriver :: MonadFlow m => SP.Person -> m Text
verifyDriver user = do
  unless ((user.role) `elem` [SP.ADMIN, SP.DRIVER]) $
    throwError AccessDenied
  case user.organizationId of
    Just orgId -> return $ getId orgId
    Nothing -> throwError (PersonFieldNotPresent "organization_id")

validateAdmin :: (DBFlow m r, EncFlow m r) => RegToken -> m Text
validateAdmin regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <-
    QP.findPersonById (Id entityId)
      >>= fromMaybeM PersonNotFound
  verifyAdmin user

validateDriver :: (DBFlow m r, EncFlow m r) => RegToken -> m Text
validateDriver regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <-
    QP.findPersonById (Id entityId)
      >>= fromMaybeM PersonNotFound
  verifyDriver user

validateAdminAction :: VerificationAction AdminVerifyToken AppEnv
validateAdminAction = VerificationAction validateAdmin

validateDriverAction :: VerificationAction DriverVerifyToken AppEnv
validateDriverAction = VerificationAction validateAdmin
