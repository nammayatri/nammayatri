{-# LANGUAGE PackageImports #-}

module Tools.Auth (verifyTokenAction, authTokenCacheKey, Common.TokenAuth) where

import Beckn.Prelude
import Beckn.Types.App
import Beckn.Utils.Common
import qualified "lib-dashboard" Tools.Auth as Common
import Tools.Servant.HeaderAuth

authTokenCacheKey :: RegToken -> Text
authTokenCacheKey regToken =
  "BAP-dashboard:authTokenCacheKey:" <> regToken

verifyTokenAction ::
  ( EsqDBFlow m r,
    HasFlowEnv m r ["authTokenCacheExpiry" ::: Seconds, "registrationTokenExpiry" ::: Days]
  ) =>
  VerificationActionWithPayload Common.VerifyToken m
verifyTokenAction = Common.verifyTokenAction authTokenCacheKey
