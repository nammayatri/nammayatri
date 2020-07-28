{-# LANGUAGE OverloadedLabels #-}

module Utils.Common where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (encodeToText')
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.Auth
import Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Servant.Client.Core.ClientError
import Servant.Client.Core.Response
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR

instance
  SanitizedUrl (sub :: *) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  SanitizedUrl (sub :: *) =>
  SanitizedUrl (OrgTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  SanitizedUrl (sub :: *) =>
  SanitizedUrl (DriverTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  SanitizedUrl (sub :: *) =>
  SanitizedUrl (AdminTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs simple token verification.
type TokenAuth = TokenAuth' "token" VerifyToken

data VerifyToken = VerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = SR.RegistrationToken
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyTokenAction :: VerificationAction VerifyToken AppEnv
verifyTokenAction = VerificationAction QR.verifyToken

-- | Verifies org's token
type OrgTokenAuth = TokenAuth' "token" OrgVerifyToken

data OrgVerifyToken = OrgVerifyToken

instance VerificationMethod OrgVerifyToken where
  type VerificationResult OrgVerifyToken = SO.Organization
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyOrgAction :: VerificationAction OrgVerifyToken AppEnv
verifyOrgAction = VerificationAction QO.verifyToken

-- | Verifies admin's token.
type AdminTokenAuth = TokenAuth' "token" AdminVerifyToken

data AdminVerifyToken

instance VerificationMethod AdminVerifyToken where
  type VerificationResult AdminVerifyToken = Text
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin role."

-- | Verifies admin or driver's token.
type DriverTokenAuth = TokenAuth' "token" DriverVerifyToken

data DriverVerifyToken

instance VerificationMethod DriverVerifyToken where
  type VerificationResult DriverVerifyToken = Text

  -- verifyToken = validateDriver
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin or driver role."

verifyAdmin :: SP.Person -> Flow Text
verifyAdmin user = do
  when (user ^. #_role /= SP.ADMIN) $
    L.throwException $
      err400 {errBody = "NEED_ADMIN_ACCESS"}
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}

verifyDriver :: SP.Person -> Flow Text
verifyDriver user = do
  unless ((user ^. #_role) `elem` [SP.ADMIN, SP.DRIVER]) $
    L.throwException $
      err400 {errBody = "NEED_ADMIN_OR_DRIVER_ACCESS"}
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}

validateAdmin :: RegToken -> Flow Text
validateAdmin regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (PersonId _EntityId)
  verifyAdmin user

validateDriver :: RegToken -> Flow Text
validateDriver regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (PersonId _EntityId)
  verifyDriver user

validateAdminAction :: VerificationAction AdminVerifyToken AppEnv
validateAdminAction = VerificationAction validateAdmin

validateDriverAction :: VerificationAction DriverVerifyToken AppEnv
validateDriverAction = VerificationAction validateAdmin

-- TODO: figure out a way to extract the url directly from EulerClient
callAPI baseUrl req serviceName = do
  endTracking <- L.runUntracedIO $ Metrics.startTracking (encodeToText' baseUrl) serviceName
  res <- L.callAPI baseUrl req
  let status = case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"
  _ <- L.runUntracedIO $ endTracking status
  return res
