{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Beckn.InternalAPI.Auth.Client where

import Beckn.InternalAPI.Auth.API
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Error.BaseError.HTTPError.APIError
import Beckn.Utils.Error.Throwing
import qualified EulerHS.Types as E

authAPI :: Text -> E.EulerClient PersonId
authAPI = E.client (Proxy @API)

auth ::
  ( HasInConfig r c "authServiceUrl" BaseUrl,
    CoreMetrics m,
    MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  m PersonId
auth token = do
  url <- askConfig (.authServiceUrl)
  callOwnAPI Nothing (Just "AUTH_FAILED") url (authAPI token) "auth"
    `catchOwnAPI` throwError . \case
      "INVALID_TOKEN" -> InvalidToken token
      "TOKEN_IS_NOT_VERIFIED" -> TokenIsNotVerified
      "TOKEN_EXPIRED" -> TokenExpired
