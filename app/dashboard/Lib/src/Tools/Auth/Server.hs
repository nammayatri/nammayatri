{-# LANGUAGE TypeApplications #-}

module Tools.Auth.Server (ServerAuth, verifyServerAction, module Reexport) where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Data.Singletons.TH
import Domain.Types.RegistrationToken as Reexport (ServerName (..))
import qualified Domain.Types.RegistrationToken as DR
import qualified Domain.Types.RegistrationToken as DReg
import Servant hiding (throwError)
import qualified Tools.Auth.Common as Common
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (ServerAuth r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking server access.
type ServerAuth sn = HeaderAuthWithPayload "token" VerifyServer (ServerPayload sn)

data VerifyServer

data ServerPayload (sn :: DReg.ServerName)

instance VerificationMethod VerifyServer where
  type VerificationResult VerifyServer = DR.ServerName
  verificationDescription =
    "Checks whether token is registered and checks person server access.\
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyServer where
  type VerificationPayloadType VerifyServer = DR.ServerName

verifyServerAction ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  VerificationActionWithPayload VerifyServer m
verifyServerAction = VerificationActionWithPayload verifyServer

verifyServer ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  DR.ServerName ->
  RegToken ->
  m DR.ServerName
verifyServer requiredServerAccess token = do
  (_personId, serverName) <- Common.verifyPerson token
  unless (requiredServerAccess == serverName) $ throwError AccessDenied
  return serverName

instance
  forall (sn :: DReg.ServerName).
  SingI sn =>
  (VerificationPayload DReg.ServerName) (ServerPayload sn)
  where
  toPayloadType _ = fromSing (sing @sn)
