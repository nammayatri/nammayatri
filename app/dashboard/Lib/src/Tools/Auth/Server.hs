{-# LANGUAGE TypeApplications #-}

module Tools.Auth.Server (ServerAuth, ServerAccess, verifyServerAction, module Reexport) where

import Beckn.Prelude
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Data.Singletons.TH
import Domain.Types.RegistrationToken as Reexport (ServerName (..))
import qualified Domain.Types.RegistrationToken as DR
import qualified Domain.Types.RegistrationToken as DReg
import Domain.Types.Role as Reexport (DashboardAccessType (..))
import Servant hiding (throwError)
import qualified Tools.Auth.Common as Common
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ServerAuth r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking server access.
type ServerAuth sn = HeaderAuthWithPayload "token" VerifyServer sn

data VerifyServer

instance VerificationMethod VerifyServer where
  type VerificationResult VerifyServer = DR.ServerName
  verificationDescription =
    "Checks whether token is registered and checks person server access.\
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyServer where
  type VerificationPayloadType VerifyServer = DR.ServerName

verifyServerAction ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  VerificationActionWithPayload VerifyServer m
verifyServerAction = VerificationActionWithPayload verifyServer

verifyServer ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  DR.ServerName ->
  RegToken ->
  m DR.ServerName
verifyServer requiredServerAccess token = do
  (_personId, serverName) <- Common.verifyPerson token
  unless (requiredServerAccess == serverName) $ throwError AccessDenied
  return serverName

-- This type is similar to DReg.ServerName, but used only on type level

data ServerAccess sn

instance
  forall (sn :: DReg.ServerName).
  SingI sn =>
  (VerificationPayload DReg.ServerName) (ServerAccess sn)
  where
  toPayloadType _ = fromSing (sing @sn)
