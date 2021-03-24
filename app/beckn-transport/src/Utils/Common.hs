{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common where

import App.Types
import Beckn.Types.App
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Crypto.Number.Generate as Cryptonite
import Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant hiding (throwError)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.Error

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

verifyAdmin :: SP.Person -> Flow Text
verifyAdmin user = do
  when (user ^. #_role /= SP.ADMIN) $
    throwError AccessDenied
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> throwError PersonOrgIdNotPresent

verifyDriver :: SP.Person -> Flow Text
verifyDriver user = do
  unless ((user ^. #_role) `elem` [SP.ADMIN, SP.DRIVER]) $
    throwError AccessDenied
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> throwError PersonOrgIdNotPresent

validateAdmin :: RegToken -> Flow Text
validateAdmin regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (Id _EntityId)
  verifyAdmin user

validateDriver :: RegToken -> Flow Text
validateDriver regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (Id _EntityId)
  verifyDriver user

validateAdminAction :: VerificationAction AdminVerifyToken AppEnv
validateAdminAction = VerificationAction validateAdmin

validateDriverAction :: VerificationAction DriverVerifyToken AppEnv
validateDriverAction = VerificationAction validateAdmin

generateOTPCode :: Flow Text
generateOTPCode =
  L.runIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999
