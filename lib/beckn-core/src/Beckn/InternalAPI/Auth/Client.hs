{-# LANGUAGE TypeApplications #-}

module Beckn.InternalAPI.Auth.Client where

import Beckn.InternalAPI.Auth.API
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Error.BaseError.HTTPError.APIError
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
